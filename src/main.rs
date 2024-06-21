use reedline::{DefaultPrompt, DefaultPromptSegment, Reedline};
use std::{fs, io::stdin};

use spyglys::{contents_to_interpreter, interpreter::Interpreter, run_input};

use clap::Parser;

/// The cli for Spyglys
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of file to run
    file: Option<String>,

    /// Name of file to run
    #[arg(short, long)]
    shell: bool,
}

fn main() {
    let args = Args::parse();
    let mut interpreter = if let Some(f) = args.file {
        let file_contents = fs::read_to_string(f).unwrap();
        contents_to_interpreter(&file_contents)
    } else {
        Interpreter::new()
    };
    if args.shell {
        let mut line_editor = Reedline::create();
        let prompt = DefaultPrompt::new(
            DefaultPromptSegment::Basic(String::new()),
            DefaultPromptSegment::CurrentDateTime,
        );
        loop {
            let sig = line_editor.read_line(&prompt);
            match sig {
                Ok(reedline::Signal::Success(buffer)) => {
                    let v = run_input(&buffer, &mut interpreter);
                    println!("{}", v)
                }
                Ok(reedline::Signal::CtrlD) | Ok(reedline::Signal::CtrlC) => {
                    println!("Exiting shell");
                    break;
                }
                _ => {}
            }
        }
    } else {
        loop {
            let mut input = String::new();
            stdin().read_line(&mut input).unwrap();
            let resp = interpreter.run_function("main", &input);
            println!("{resp}");
        }
    }
}
