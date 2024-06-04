use std::{collections::BTreeMap, path::PathBuf, rc::Rc};

use anyhow::Result;
use clap::{arg, command, Parser as ClapParser};
use nvc::{compiler::{module::ExpandedModuleTree, Compiler}, lexer::Lexer, parser::Parser};

#[derive(ClapParser, Debug)]
#[command(author, version, about="Compiler for NVLang", long_about = None)]
struct Cli {
    input: PathBuf,
    #[arg(long, short)]
    output: String,
}

fn main() -> Result<()> {
    let cli = Cli::try_parse()?;
    let filename: Option<Rc<str>> = cli
        .input
        .file_name()
        .map(|f| f.to_string_lossy().into());
    let source = std::fs::read_to_string(cli.input)?;
    let lexer = Lexer::new(source.chars(), filename.clone());
    let token_stream = lexer.tokenize()?;
    let parser = Parser::new(token_stream, filename);
    let module = parser.parse_module().map_err(|e| std::io::Error::other(e.to_string()))?;
    dbg!(&module);
    let module_tree = ExpandedModuleTree {
        modules: BTreeMap::new(),
        uses: module.uses,
        extern_uses: module.extern_uses,
        entries: module.entries,
        functions: module.functions,
    };
    let mut compiler = Compiler::new(module_tree);
    let modules = compiler.build().map_err(|e| std::io::Error::other(e.to_string()))?;
    Ok(())
}
