use reedline::{DefaultPrompt, DefaultPromptSegment, Reedline};
use std::{fs, io::stdin};

use spyglys::{
    contents_to_interpreter, formatter::pretty_file, interpreter::Interpreter, parse_string,
    run_input,
};

use clap::{Parser, Subcommand};

/// The cli for Spyglys
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// runs a file
    Run {
        /// Name of file to run
        file: String,
    },

    /// runs a shell
    Shell {
        /// file to use as starting point for shell
        file: Option<String>,
    },

    /// runs a shell
    Format {
        /// file to format
        file: String,
    },
}

fn main() {
    let args = Args::parse();
    match args.command {
        Commands::Shell { file } => {
            let mut interpreter = if let Some(f) = file {
                let file_contents = fs::read_to_string(f).unwrap();
                contents_to_interpreter(&file_contents)
            } else {
                Interpreter::new()
            };
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
        }
        Commands::Run { file } => {
            let file_contents = fs::read_to_string(file).unwrap();
            let interpreter = contents_to_interpreter(&file_contents);
            loop {
                let mut input = String::new();
                stdin().read_line(&mut input).unwrap();
                let resp = interpreter.run_function("main", &input);
                println!("{resp}");
            }
        }
        Commands::Format { file } => {
            let file_contents = fs::read_to_string(&file).unwrap();
            let statements = parse_string(&file_contents);
            let formatted = pretty_file(&statements);
            fs::write(format!("{file}.bak"), file_contents).unwrap();
            fs::write(file, formatted).unwrap();
        }
    }
}
