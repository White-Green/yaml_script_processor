use std::collections::{BTreeMap, HashMap, VecDeque};
use std::convert::TryInto;
use std::{fs, io};
use std::ffi::OsStr;
use std::fs::File;
use std::io::{stdout, BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use clap::{crate_authors, crate_description, crate_name, crate_version, Arg};
use hound::WavReader;
use itertools::Itertools;
use regex::Regex;
use serde::Deserialize;
use serde_derive::Deserialize;
use serde_yaml::{Mapping, Number, Value};

#[derive(Debug, Clone, Copy, Deserialize, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[allow(clippy::upper_case_acronyms)]
enum TTSPlatforms {
    #[serde(alias = "VOICEROID2")]
    #[serde(alias = "VOICEROID 2")]
    Voiceroid2,
    #[serde(alias = "CeVIOAI")]
    #[serde(alias = "CeVIO AI")]
    CeVIOAI,
    #[serde(alias = "AIVOICE")]
    #[serde(alias = "A.I.VOICE")]
    AIVOICE,
}

struct PlatformSpecifiedData {
    voiceroid2_directory: Option<PathBuf>,
    voiceroid2_name_prefix: String,
    voiceroid2_script_buffer: String,
    voiceroid2_script_output: BufWriter<Box<dyn Write>>,
    cevioai_directory: Option<PathBuf>,
    cevioai_wav_file_names: Vec<PathBuf>,
    cevioai_script_buffer: String,
    cevioai_script_output: BufWriter<Box<dyn Write>>,
    aivoice_directory: Option<PathBuf>,
    aivoice_name_prefix: String,
    aivoice_script_buffer: String,
    aivoice_script_output: BufWriter<Box<dyn Write>>,
}

fn iter_wav_file_path_in_directory(
    directory: impl AsRef<Path>,
) -> io::Result<impl Iterator<Item=PathBuf>> {
    Ok(fs::read_dir(directory).unwrap()
        .filter_map(|dir| dir.ok().map(|e| e.path()))
        .filter(|path| {
            path.extension()
                .map(|extension| extension == "wav")
                .unwrap_or(false)
        }))
}

fn set_for_aitalk(
    directory: impl AsRef<Path>,
    directory_path: &mut Option<PathBuf>,
    script_output: &mut BufWriter<Box<dyn Write>>,
    name_prefix: &mut String,
) -> anyhow::Result<()> {
    fs::create_dir_all(&directory).unwrap();
    let script_output_file = File::create(directory.as_ref().join("script.txt"))?;
    let regex = Regex::new("-\\d+\\.wav$").unwrap();
    let wav_file_name_prefix = iter_wav_file_path_in_directory(&directory)?
        .filter_map(|path| {
            path.file_name()
                .and_then(OsStr::to_str)
                .map(|file_name| regex.replace(file_name, "").into_owned())
        })
        .dedup()
        .collect::<Vec<_>>();
    let [wav_file_name_prefix]: [_; 1] = wav_file_name_prefix
        .try_into()
        .unwrap_or_else(|_| [String::new()]);
    *directory_path = Some(directory.as_ref().to_path_buf());
    *script_output = BufWriter::new(Box::new(script_output_file));
    *name_prefix = wav_file_name_prefix;
    Ok(())
}

impl PlatformSpecifiedData {
    fn new() -> Self {
        Self {
            voiceroid2_directory: None,
            voiceroid2_name_prefix: Default::default(),
            voiceroid2_script_buffer: Default::default(),
            voiceroid2_script_output: BufWriter::new(Box::new(stdout())),
            cevioai_directory: None,
            cevioai_wav_file_names: Default::default(),
            cevioai_script_buffer: Default::default(),
            cevioai_script_output: BufWriter::new(Box::new(stdout())),
            aivoice_directory: None,
            aivoice_name_prefix: Default::default(),
            aivoice_script_buffer: Default::default(),
            aivoice_script_output: BufWriter::new(Box::new(stdout())),
        }
    }

    fn set_for_voiceroid2(&mut self, directory: impl AsRef<Path>) -> anyhow::Result<()> {
        set_for_aitalk(
            directory,
            &mut self.voiceroid2_directory,
            &mut self.voiceroid2_script_output,
            &mut self.voiceroid2_name_prefix,
        )
    }

    fn set_for_cevioai(&mut self, directory: impl AsRef<Path>) -> anyhow::Result<()> {
        fs::create_dir_all(&directory).unwrap();
        let script_output = File::create(directory.as_ref().join("script.csv")).unwrap();
        let wav_file_names = iter_wav_file_path_in_directory(&directory).unwrap()
            .filter_map(|path| path.file_name().map(PathBuf::from))
            .collect::<Vec<_>>();
        self.cevioai_directory = Some(directory.as_ref().to_path_buf());
        self.cevioai_script_output = BufWriter::new(Box::new(script_output));
        self.cevioai_wav_file_names = wav_file_names;
        Ok(())
    }

    fn set_for_aivoice(&mut self, directory: impl AsRef<Path>) -> anyhow::Result<()> {
        set_for_aitalk(
            directory,
            &mut self.aivoice_directory,
            &mut self.aivoice_script_output,
            &mut self.aivoice_name_prefix,
        )
    }

    fn push_script(&mut self, character_name: &str, platform: TTSPlatforms, script: &str) {
        match platform {
            TTSPlatforms::Voiceroid2 => {
                if !self.voiceroid2_script_buffer.is_empty() {
                    self.voiceroid2_script_buffer.push_str("/\n");
                }
                self.voiceroid2_script_buffer.push_str(character_name);
                self.voiceroid2_script_buffer.push('＞');
                self.voiceroid2_script_buffer.push_str(script);
            }
            TTSPlatforms::CeVIOAI => {
                self.cevioai_script_buffer.push_str(character_name);
                self.cevioai_script_buffer.push(',');
                self.cevioai_script_buffer.push_str(script);
                self.cevioai_script_buffer.push('\n');
            }
            TTSPlatforms::AIVOICE => {
                if !self.aivoice_script_buffer.is_empty() {
                    self.aivoice_script_buffer.push_str("/\n");
                }
                self.aivoice_script_buffer.push_str(character_name);
                self.aivoice_script_buffer.push('＞');
                self.aivoice_script_buffer.push_str(script);
            }
        }
    }

    fn write_all(mut self) -> io::Result<()> {
        self.voiceroid2_script_output.write_all(
            &encoding_rs::SHIFT_JIS
                .encode(&self.voiceroid2_script_buffer)
                .0,
        )?;
        self.cevioai_script_output
            .write_all(self.cevioai_script_buffer.as_bytes())?;
        self.aivoice_script_output
            .write_all(&encoding_rs::SHIFT_JIS.encode(&self.aivoice_script_buffer).0)?;
        Ok(())
    }

    fn get_wav_file_name(&self, platform: TTSPlatforms, n: usize) -> String {
        match platform {
            TTSPlatforms::Voiceroid2 => format!("{}-{}.wav", self.voiceroid2_name_prefix, n),
            TTSPlatforms::CeVIOAI => self.cevioai_wav_file_names.get(n).map(|name| name.display().to_string()).unwrap_or_else(|| format!("{n}.wav")),
            TTSPlatforms::AIVOICE => format!("{}-{}.wav", self.aivoice_name_prefix, n),
        }
    }

    fn get_lab_file_name(&self, platform: TTSPlatforms, n: usize) -> String {
        match platform {
            TTSPlatforms::Voiceroid2 => format!("{}-{}.lab", self.voiceroid2_name_prefix, n),
            TTSPlatforms::CeVIOAI => self.cevioai_wav_file_names.get(n).map(|name| name.with_extension("lab").display().to_string()).unwrap_or_else(|| format!("{n}.lab")),
            TTSPlatforms::AIVOICE => format!("{}-{}.lab", self.aivoice_name_prefix, n),
        }
    }

    fn get_directory_path(&self, platform: TTSPlatforms) -> Option<&Path> {
        match platform {
            TTSPlatforms::Voiceroid2 => self.voiceroid2_directory.as_deref(),
            TTSPlatforms::CeVIOAI => self.cevioai_directory.as_deref(),
            TTSPlatforms::AIVOICE => self.aivoice_directory.as_deref(),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
struct CharacterProperties {
    name: Option<String>,
    platform: TTSPlatforms,
    #[serde(default)]
    template: serde_yaml::Value,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
enum NormalScriptValue {
    Script(String),
    ScriptAndSubtitleTagged { voice: String, subtitle: String },
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
enum Script {
    NormalScript(NormalScriptValue),
    NormalScriptList(Vec<NormalScriptValue>),
    ExVoice { path: PathBuf, subtitle: String },
}

impl Script {
    fn normalize(self) -> Script {
        match self {
            Script::NormalScript(script) => Script::NormalScriptList(vec![script]),
            script => script,
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
struct CharacterSettings(BTreeMap<String, CharacterProperties>);

fn main() {
    let matches = clap::app_from_crate!()
        .arg(
            Arg::with_name("script")
                .short("s")
                .long("script")
                .help("path/to/script.yaml")
                .takes_value(true)
                .required(true),
        )
        .arg(
            Arg::with_name("typescript_output")
                .long("typescript")
                .help("path/to/typescript_file for voice output")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("json_output")
                .long("json")
                .help("path/to/json_file for script json output")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("voiceroid2_directory")
                .long("voiceroid2")
                .help("path/to/directory for VOICEROID2")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("cevioai_directory")
                .long("cevioai")
                .help("path/to/directory for CeVIO AI")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("aivoice_directory")
                .long("aivoice")
                .help("path/to/directory for A.I.VOICE")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("exvoice_directory")
                .long("exvoice")
                .help("path/to/directory for temporary for exvoice")
                .takes_value(true),
        )
        .get_matches();
    let script_path = matches.value_of("script").unwrap();
    let script = fs::read_to_string(script_path).expect("failed to read script.yaml");
    let documents = serde_yaml::Deserializer::from_str(&script)
        .map(|d| serde_yaml::Value::deserialize(d).expect("failed parsing yaml"))
        .collect::<Vec<_>>();
    assert_eq!(documents.len(), 2, "script.yaml should have 2 documents.");
    let [character_settings, script]: [_; 2] = documents.try_into().unwrap();
    let CharacterSettings(character_settings) =
        serde_yaml::from_value::<CharacterSettings>(character_settings)
            .expect("the 1st document should have character settings.");
    let script = serde_yaml::from_value::<Vec<BTreeMap<String, Script>>>(script)
        .expect("the 2nd document should have scripts.");
    let script = script
        .into_iter()
        .map(|map| {
            assert_eq!(map.len(), 1, "the 2nd document should have scripts.");
            let [(character_name, script)]: [_; 1] =
                map.into_iter().collect::<Vec<_>>().try_into().unwrap();
            assert!(character_settings.contains_key(&character_name));
            (character_name, script)
        })
        .collect::<Vec<_>>();

    let mut typescript_output = matches
        .value_of("typescript_output")
        .map(|path| {
            let path = Path::new(path);
            fs::create_dir_all(path.parent().unwrap())?;
            Ok::<_, io::Error>((AsRef::<Path>::as_ref(path), File::create(path)?))
        })
        .transpose()
        .unwrap_or_default();
    let mut json_output = matches
        .value_of("json_output")
        .map(File::create)
        .transpose()
        .unwrap_or_default();

    let mut platform_specified_data = PlatformSpecifiedData::new();
    if let Some(voiceroid2_directory) = matches.value_of("voiceroid2_directory") {
        platform_specified_data.set_for_voiceroid2(voiceroid2_directory).unwrap();
    }
    if let Some(cevioai_directory) = matches.value_of("cevioai_directory") {
        platform_specified_data.set_for_cevioai(cevioai_directory).unwrap();
    }
    if let Some(aivoice_directory) = matches.value_of("aivoice_directory") {
        platform_specified_data.set_for_aivoice(aivoice_directory).unwrap();
    }

    let mut sout = stdout();
    let (typescript_path, typescript_output) = typescript_output
        .as_mut()
        .map(|(p, f)| (*p, f as &mut dyn Write))
        .unwrap_or((AsRef::<Path>::as_ref("."), &mut sout as &mut dyn Write));
    generate(
        character_settings,
        script,
        typescript_output,
        typescript_path,
        &mut platform_specified_data,
        json_output
            .as_mut()
            .map(|f| f as &mut dyn Write)
            .unwrap_or(&mut stdout() as &mut dyn Write),
        matches.value_of("exvoice_directory"),
    );
    platform_specified_data.write_all().unwrap();
}

fn generate(
    character_settings: BTreeMap<String, CharacterProperties>,
    script: Vec<(String, Script)>,
    typescript_output: impl Write,
    typescript_path: &Path,
    platform_specified_data: &mut PlatformSpecifiedData,
    json_output: impl Write,
    exvoice_directory: Option<impl AsRef<Path>>,
) {
    let mut json_output = BufWriter::new(json_output);
    let mut typescript_output = BufWriter::new(typescript_output);
    let exvoice_directory = exvoice_directory.as_ref().map(AsRef::as_ref);
    if let Some(exvoice_directory) = exvoice_directory {
        fs::create_dir_all(exvoice_directory).unwrap();
    }

    let typescript_path = typescript_path.parent().unwrap();
    let mut wav_imports = Vec::new();
    let mut wav_length = Vec::new();
    let mut wav_values = Vec::new();

    let mut subtitle_template = Vec::new();
    let mut wav_indices = HashMap::<_, usize>::new();
    for (index, (character_name, script)) in script.into_iter().enumerate() {
        let character_property = character_settings.get(&character_name).unwrap();
        let (wav_path, wav_full_path, same_platform_wav_index, platform_name, lab_file_path, subtitle) = match script.normalize() {
            Script::NormalScript(_) => unreachable!(),
            Script::NormalScriptList(scripts) => {
                let (script, subtitle) = scripts.iter().fold((String::new(), String::new()), |(mut scripts, mut subtitles), script| {
                    match script {
                        NormalScriptValue::Script(script) => {
                            scripts.push_str(script);
                            subtitles.push_str(script);
                        }
                        NormalScriptValue::ScriptAndSubtitleTagged { voice, subtitle } => {
                            scripts.push_str(voice);
                            subtitles.push_str(subtitle);
                        }
                    }
                    (scripts, subtitles)
                });
                platform_specified_data.push_script(
                    character_property.name.as_ref().unwrap_or(&character_name),
                    character_property.platform,
                    &script,
                );
                let wav_index = wav_indices
                    .entry(Some(character_property.platform))
                    .or_default();
                let current_wav_index = *wav_index;
                *wav_index += 1;
                let directory = platform_specified_data
                    .get_directory_path(character_property.platform)
                    .unwrap_or_else(|| AsRef::<Path>::as_ref("."));
                let wav_file_name =
                    platform_specified_data.get_wav_file_name(character_property.platform, current_wav_index);
                let lab_file_path = directory.join(
                    &platform_specified_data.get_lab_file_name(character_property.platform, current_wav_index),
                );
                let wav_full_path = directory.join(&wav_file_name);
                let wav_path =
                    pathdiff::diff_paths(&wav_full_path, typescript_path).unwrap();
                (wav_path, wav_full_path, current_wav_index, format!("{:?}", character_property.platform), Some(lab_file_path), subtitle)
            }
            Script::ExVoice { path, subtitle } => {
                let wav_index = wav_indices
                    .entry(None)
                    .or_default();
                let current_wav_index = *wav_index;
                *wav_index += 1;
                let path = if let Some(exvoice_directory) = exvoice_directory {
                    let new_file_name = path.to_string_lossy().replace(['\\', '/', ':', '*', '?', '"', '<', '>', '|'], "_");
                    let new_path = exvoice_directory.join(&new_file_name);
                    fs::copy(&path, &new_path).unwrap();
                    new_path
                } else {
                    path
                };
                (pathdiff::diff_paths(&path, typescript_path).unwrap(), path, current_wav_index, "ExVoice".to_string(), None, subtitle)
            }
        };
        wav_imports.push(format!(
            "import {}_WAV_{} from {:?};",
            platform_name,
            same_platform_wav_index,
            format!(
                "./{}",
                wav_path
                    .to_string_lossy()
                    .replace(std::path::MAIN_SEPARATOR, "/")
            )
        ));
        wav_values.push(format!(
            "{}_WAV_{}",
            platform_name, same_platform_wav_index
        ));
        let wav_len = WavReader::open(&wav_full_path)
            .map(|reader| reader.duration() as f64 / reader.spec().sample_rate as f64)
            .unwrap_or_default();
        wav_length.push(wav_len.to_string());
        let mut value = character_property.template.clone();
        let mut q = VecDeque::new();
        q.push_back(&mut value);
        while let Some(value) = q.pop_front() {
            match value {
                Value::String(s) => match s.trim() {
                    "${script_index}" => *value = Value::Number(Number::from(index)),
                    "${lab}" => *value = lab_file_path.as_ref().and_then(|lab_file_path| get_lab_value(&lab_file_path)).unwrap_or(Value::Null),
                    _ => *s = s.replace("${script}", &subtitle),
                },
                Value::Sequence(values) => q.extend(values.iter_mut()),
                Value::Mapping(values) => q.extend(values.iter_mut().map(|(_, v)| v)),
                _ => {}
            }
        }
        subtitle_template.push(value);
    }
    let wav_imports = wav_imports.join(" ");
    let wav_values = format!("export const WAV_VALUES = [{}];", wav_values.join(", "));
    let wav_length = format!("export const WAV_LENGTH = [{}];", wav_length.join(", "));

    typescript_output
        .write_all(format!("{}\n{}\n{}", wav_imports, wav_values, wav_length).as_bytes())
        .expect("failed to write typescript file");
    drop(typescript_output);
    json_output
        .write_all(
            serde_json::to_string_pretty(&subtitle_template)
                .unwrap()
                .as_bytes(),
        )
        .expect("failed to write generated json");
    drop(json_output);
}

fn get_lab_value(lab_file_path: &Path) -> Option<Value> {
    let file = BufReader::new(File::open(lab_file_path).ok()?);
    let mut result = Vec::new();
    for line in file.lines() {
        let line = line.ok()?;
        let [begin, end, phoneme]: [_; 3] = line
            .split_whitespace()
            .collect::<Vec<_>>()
            .try_into()
            .ok()?;
        let mut map = Mapping::with_capacity(3);
        map.insert(
            Value::String("begin".to_string()),
            Value::Number(Number::from(<i64>::from_str(begin).ok()?)),
        );
        map.insert(
            Value::String("end".to_string()),
            Value::Number(Number::from(<i64>::from_str(end).ok()?)),
        );
        map.insert(
            Value::String("phoneme".to_string()),
            Value::String(phoneme.to_string()),
        );
        result.push(Value::Mapping(map));
    }
    Some(Value::Sequence(result))
}
