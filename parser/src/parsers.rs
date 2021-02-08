use std::convert::TryFrom;

use crate::ast::ModuleStmt::*;
use crate::ast::*;
use crate::builders::{
    alt,
    delimited,
    many0,
    many1,
    map,
    op_expr_builder,
    opt,
    pair,
    preceded,
    separated,
    terminated,
    verify,
};
use crate::errors::ParseError;
use crate::span::{
    Span,
    Spanned,
};
use crate::tokenizer::types::{
    Token,
    TokenType,
};
use crate::{
    Cursor,
    ParseResult,
};

/// Parse next token in input.
pub fn next(input: Cursor) -> ParseResult<&Token> {
    match input.first() {
        Some(tok) => Ok((&input[1..], tok)),
        None => Err(ParseError::eof(input)),
    }
}

/// Parse a token of a specific type.
pub fn token<'a>(typ: TokenType) -> impl Fn(Cursor<'a>) -> ParseResult<&Token> {
    verify(
        next,
        move |t| t.typ == typ,
        move |inp, _| ParseError::str(inp, format!("expected {:?} token", typ)),
    )
}

/// Parse a name token.
pub fn name_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::NAME)(input)
}

/// Parse a name token containing a specific string.
#[allow(clippy::needless_lifetimes)]
pub fn name<'a>(string: &'a str) -> impl Fn(Cursor<'a>) -> ParseResult<&Token> {
    verify(
        name_token,
        move |t| t.string == string,
        move |inp, _| ParseError::str(inp, format!("expected \"{}\" name token", string)),
    )
}

/// Parse an op token.
pub fn op_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::OP)(input)
}

/// Parse an op token containing a specific string.
#[allow(clippy::needless_lifetimes)]
pub fn op<'a>(string: &'a str) -> impl Fn(Cursor<'a>) -> ParseResult<&Token> {
    verify(
        op_token,
        move |t| t.string == string,
        move |inp, _| ParseError::str(inp, format!("expected \"{}\" op token", string)),
    )
}

/// Parse a number token.
pub fn number_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::NUMBER)(input)
}

/// Parse a string token.
pub fn string_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::STRING)(input)
}

/// Parse an indent token.
pub fn indent_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::INDENT)(input)
}

/// Parse a dedent token.
pub fn dedent_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::DEDENT)(input)
}

/// Parse a grammatically significant newline token.
pub fn newline_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::NEWLINE)(input)
}

/// Parse an endmarker token.
pub fn endmarker_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::ENDMARKER)(input)
}

/// Parse a module definition.
pub fn file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    alt((empty_file_input, non_empty_file_input))(input)
}

/// Parse an empty module definition.
pub fn empty_file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    let (input, newline_tok) = newline_token(input)?;
    let (input, _) = endmarker_token(input)?;

    Ok((
        input,
        Spanned {
            node: Module { body: vec![] },
            span: newline_tok.span,
        },
    ))
}

/// Parse a non-empty module definition.
pub fn non_empty_file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    // module_stmt+
    let (input, body) = many1(module_stmt)(input)?;

    // ENDMARKER
    let (input, _) = endmarker_token(input)?;

    let span = {
        let first = body.first().unwrap();
        let last = body.last().unwrap();

        Span::from_pair(first, last)
    };

    Ok((
        input,
        Spanned {
            node: Module { body },
            span,
        },
    ))
}

/// Parse a module statement, such as a contract definition.
pub fn module_stmt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    alt((import_stmt, type_def, contract_def))(input)
}

/// Parse an import statement.
pub fn import_stmt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    terminated(alt((simple_import, from_import)), newline_token)(input)
}

/// Parse an import statement beginning with the "import" keyword.
pub fn simple_import(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, import_kw) = name("import")(input)?;
    let (input, first_name) = simple_import_name(input)?;
    let (input, mut other_names) = many0(preceded(op(","), simple_import_name))(input)?;

    let mut result = vec![first_name];
    result.append(&mut other_names);

    let span = {
        let last = result.last().unwrap();
        Span::from_pair(import_kw, last)
    };

    Ok((
        input,
        Spanned {
            node: SimpleImport { names: result },
            span,
        },
    ))
}

pub fn simple_import_name(input: Cursor) -> ParseResult<Spanned<SimpleImportName>> {
    let (input, path) = dotted_name(input)?;
    let (input, alias) = opt(preceded(name("as"), name_token))(input)?;

    let first = path.first().unwrap();
    let last = path.last().unwrap();
    let path_span = Span::from_pair(first, last);

    let span = {
        match alias {
            Some(alias_tok) => Span::from_pair(path_span, alias_tok),
            None => path_span,
        }
    };

    Ok((
        input,
        Spanned {
            node: SimpleImportName {
                path,
                alias: alias.map(|t| t.into()),
            },
            span,
        },
    ))
}

/// Parse an import statement beginning with the "from" keyword.
pub fn from_import(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    alt((from_import_parent_alt, from_import_sub_alt))(input)
}

/// Parse a "from" import with a path that contains only parent module
/// components.
pub fn from_import_parent_alt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, from_kw) = name("from")(input)?;
    let (input, parent_level) = dots_to_int(input)?;
    let (input, _) = name("import")(input)?;
    let (input, names) = from_import_names(input)?;

    let path = Spanned {
        node: FromImportPath::Relative {
            parent_level: parent_level.node,
            path: vec![],
        },
        span: parent_level.span,
    };
    let span = Span::from_pair(from_kw, names.span);

    Ok((
        input,
        Spanned {
            node: FromImport { path, names },
            span,
        },
    ))
}

/// Parse a "from" import with a path that contains sub module components.
pub fn from_import_sub_alt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, from_kw) = name("from")(input)?;
    let (input, path) = from_import_sub_path(input)?;
    let (input, _) = name("import")(input)?;
    let (input, names) = from_import_names(input)?;

    let span = Span::from_pair(from_kw, names.span);

    Ok((
        input,
        Spanned {
            node: FromImport { path, names },
            span,
        },
    ))
}

/// Parse a path containing sub module components in a "from" import statement.
pub fn from_import_sub_path(input: Cursor) -> ParseResult<Spanned<FromImportPath>> {
    let (input, opt_parent_level) = opt(dots_to_int)(input)?;
    let (input, path) = dotted_name(input)?;

    let first = path.first().unwrap();
    let last = path.last().unwrap();
    let span = Span::from_pair(first, last);

    let result = match opt_parent_level {
        Some(parent_level) => {
            let span = Span::from_pair(&parent_level, span);
            Spanned {
                node: FromImportPath::Relative {
                    parent_level: parent_level.node,
                    path,
                },
                span,
            }
        }
        None => Spanned {
            node: FromImportPath::Absolute { path },
            span,
        },
    };

    Ok((input, result))
}

/// Parse the names to be imported by a "from" import statement.
pub fn from_import_names(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    alt((
        from_import_names_star,
        from_import_names_parens,
        from_import_names_list,
    ))(input)
}

/// Parse a wildcard token ("*") in a "from" import statement.
pub fn from_import_names_star(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, star) = op("*")(input)?;

    Ok((
        input,
        Spanned {
            node: FromImportNames::Star,
            span: star.span,
        },
    ))
}

/// Parse a parenthesized list of names to be imported by a "from" import
/// statement.
pub fn from_import_names_parens(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, l_paren) = op("(")(input)?;
    let (input, names) = from_import_names_list(input)?;
    let (input, r_paren) = op(")")(input)?;

    Ok((
        input,
        Spanned {
            node: names.node,
            span: Span::from_pair(l_paren, r_paren),
        },
    ))
}

/// Parse a list of names to be imported by a "from" import statement.
pub fn from_import_names_list(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, first_name) = from_import_name(input)?;
    let (input, mut other_names) = many0(preceded(op(","), from_import_name))(input)?;
    let (input, comma_tok) = opt(op(","))(input)?;

    let mut names = vec![first_name];
    names.append(&mut other_names);

    let span = {
        let first = names.first().unwrap();
        match comma_tok {
            Some(tok) => Span::from_pair(first, tok),
            None => {
                let last = names.last().unwrap();
                Span::from_pair(first, last)
            }
        }
    };

    Ok((
        input,
        Spanned {
            node: FromImportNames::List(names),
            span,
        },
    ))
}

/// Parse an import name with an optional alias in a "from" import statement.
pub fn from_import_name(input: Cursor) -> ParseResult<Spanned<FromImportName>> {
    let (input, name_tok) = name_token(input)?;
    let (input, alias) = opt(preceded(name("as"), name_token))(input)?;

    let span = match alias {
        Some(alias_tok) => Span::from_pair(name_tok, alias_tok),
        None => name_tok.span,
    };

    Ok((
        input,
        Spanned {
            node: FromImportName {
                name: name_tok.into(),
                alias: alias.map(|t| t.into()),
            },
            span,
        },
    ))
}

/// Parse a dotted import name.
pub fn dotted_name(input: Cursor) -> ParseResult<Vec<Spanned<&str>>> {
    separated(map(name_token, |t| t.into()), op("."), false)(input)
}

/// Parse preceding dots used to indicate parent module imports in import
/// statements.
pub fn dots_to_int(input: Cursor) -> ParseResult<Spanned<usize>> {
    let (input, toks) = many1(alt((op("."), op("..."))))(input)?;

    let value = toks
        .iter()
        .map(|t| if t.string == "." { 1 } else { 3 })
        .sum::<usize>()
        - 1;

    let span = {
        let first = toks.first().unwrap();
        let last = toks.last().unwrap();

        Span::from_pair(*first, *last)
    };

    Ok((input, Spanned { node: value, span }))
}

/// Parse a contract definition statement.
pub fn contract_def(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    // "contract" name ":" NEWLINE
    let (input, contract_kw) = name("contract")(input)?;
    let (input, name_tok) = name_token(input)?;
    let (input, _) = op(":")(input)?;
    let (input, _) = newline_token(input)?;

    // INDENT contract_stmt+ DEDENT
    let (input, _) = indent_token(input)?;
    let (input, body) = many1(contract_stmt)(input)?;
    let (input, _) = dedent_token(input)?;

    let last_stmt = body.last().unwrap();
    let span = Span::from_pair(contract_kw, last_stmt);

    Ok((
        input,
        Spanned {
            node: ContractDef {
                name: name_tok.into(),
                body,
            },
            span,
        },
    ))
}

/// Parse a contract statement.
pub fn contract_stmt(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    alt((contract_field, event_def, func_def))(input)
}

/// Parse a contract field const.
pub fn contract_field_const(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {

    let (input, (qual, name_tok)) = 
        map(pair(contract_field_const_qual, name_token), |res| {
            let (qual, tok) = res;
            (Some(qual), tok)
        })(input)?;

    let (input, _) = op(":")(input)?;
    let (input, typ) = type_desc(input)?;
    // Parse the optional assignment over here
    let (input, _ ) = op("=")(input)?;
    let (input, value) = opt(preceded(op("="), expr))(input)?;

    let span = match &qual {
        Some(spanned) => Span::from_pair(spanned, &typ),
        None => Span::from_pair(name_tok, &typ),
    };

    Ok((
        input,
        Spanned {
            node: ContractStmt::ContractField {
                qual,
                name: name_tok.into(),
                typ,
            },
            span,
        },
    ))
}

/// Parse a contract field pub.
pub fn contract_field_pub(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    let (input, (qual, name_tok)) = alt((
        // Look for a qualifier and field name first...
        map(pair(contract_field_pub_qual, name_token), |res| {
            let (qual, tok) = res;
            (Some(qual), tok)
        }),
        // ...then fall back to just a field name
        map(name_token, |tok| (None, tok)),
    ))(input)?;

    let (input, _) = op(":")(input)?;
    let (input, typ) = type_desc(input)?;
    let (input, _) = newline_token(input)?;

    let span = match &qual {
        Some(spanned) => Span::from_pair(spanned, &typ),
        None => Span::from_pair(name_tok, &typ),
    };

    Ok((
        input,
        Spanned {
            node: ContractStmt::ContractField {
                qual,
                name: name_tok.into(),
                typ,
            },
            span,
        },
    ))
}

/// Parse a contract field definition.
pub fn contract_field(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    alt((contract_field_const, contract_field_pub))(input)
}

/// Parse an event definition statement.
pub fn event_def(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    // "event" name ":" NEWLINE
    let (input, event_kw) = name("event")(input)?;
    let (input, name_tok) = name_token(input)?;
    let (input, _) = op(":")(input)?;
    let (input, _) = newline_token(input)?;

    // INDENT event_field+ DEDENT
    let (input, _) = indent_token(input)?;
    let (input, fields) = many1(event_field)(input)?;
    let (input, _) = dedent_token(input)?;

    let last_field = fields.last().unwrap();
    let span = Span::from_pair(event_kw, last_field);

    Ok((
        input,
        Spanned {
            node: ContractStmt::EventDef {
                name: name_tok.into(),
                fields,
            },
            span,
        },
    ))
}

/// Parse an event field definition.
pub fn event_field(input: Cursor) -> ParseResult<Spanned<EventField>> {
    let (input, (qual, name_tok)) = alt((
        // Look for a qualifier and field name first...
        map(pair(event_field_qual, name_token), |res| {
            let (qual, tok) = res;
            (Some(qual), tok)
        }),
        // ...then fall back to just a field name
        map(name_token, |tok| (None, tok)),
    ))(input)?;

    let (input, _) = op(":")(input)?;
    let (input, typ) = type_desc(input)?;
    let (input, _) = newline_token(input)?;

    let span = match &qual {
        Some(spanned) => Span::from_pair(spanned, &typ),
        None => Span::from_pair(name_tok, &typ),
    };

    Ok((
        input,
        Spanned {
            node: EventField {
                qual,
                name: name_tok.into(),
                typ,
            },
            span,
        },
    ))
}

pub fn func_def(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    let (input, qual) = opt(func_qual)(input)?;
    let (input, def_kw) = name("def")(input)?;
    let (input, name_tok) = name_token(input)?;

    let (input, _) = op("(")(input)?;
    let (input, args) = arg_list(input)?;
    let (input, _) = op(")")(input)?;

    let (input, return_type) = opt(preceded(op("->"), base_type))(input)?;

    let (input, _) = op(":")(input)?;

    let (input, body) = block(input)?;

    let last = body.last().unwrap();
    let span = match &qual {
        Some(qual) => Span::from_pair(qual, last),
        None => Span::from_pair(def_kw, last),
    };

    Ok((
        input,
        Spanned {
            node: ContractStmt::FuncDef {
                qual,
                name: name_tok.into(),
                args,
                return_type,
                body,
            },
            span,
        },
    ))
}

pub fn arg_list(input: Cursor) -> ParseResult<Vec<Spanned<FuncDefArg>>> {
    match input[0] {
        Token { string: ")", .. } => Ok((input, vec![])),
        _ => separated(arg_def, op(","), true)(input),
    }
}

pub fn arg_def(input: Cursor) -> ParseResult<Spanned<FuncDefArg>> {
    let (input, name_tok) = name_token(input)?;
    let (input, _) = op(":")(input)?;
    let (input, typ) = type_desc(input)?;

    let span = Span::from_pair(name_tok, &typ);

    Ok((
        input,
        Spanned {
            node: FuncDefArg {
                name: name_tok.into(),
                typ,
            },
            span,
        },
    ))
}

/// Parse a type definition (type alias).
pub fn type_def(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, type_kw) = name("type")(input)?;
    let (input, name) = name_token(input)?;
    let (input, _) = op("=")(input)?;
    let (input, type_desc) = type_desc(input)?;
    let (input, _) = newline_token(input)?;

    let span = Span::from_pair(type_kw, &type_desc);

    Ok((
        input,
        Spanned {
            node: ModuleStmt::TypeDef {
                name: name.into(),
                typ: type_desc,
            },
            span,
        },
    ))
}

/// Parse a type description e.g. "u256" or "map<address, bool>".
pub fn type_desc(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    alt((map_type, base_type))(input)
}

/// Parse a map type e.g. "map<address, bool".
pub fn map_type(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    alt((map_type_double, map_type_single))(input)
}

/// Parse a map type ending with a right-shift token.
///
/// Example:
/// map<address, map<u256, bool>>
pub fn map_type_double(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, map_kw_1) = name("map")(input)?;
    let (input, _) = op("<")(input)?;
    let (input, from_1) = base_type(input)?;
    let (input, _) = op(",")(input)?;

    let (input, map_kw_2) = name("map")(input)?;
    let (input, _) = op("<")(input)?;
    let (input, from_2) = base_type(input)?;
    let (input, _) = op(",")(input)?;

    let (input, to) = type_desc(input)?;
    let (input, r_bracket) = op(">>")(input)?;

    let inner_map = Spanned {
        node: TypeDesc::Map {
            from: Box::new(from_2),
            to: Box::new(to),
        },
        span: Span::new(map_kw_2.span.start, r_bracket.span.end - 1),
    };

    Ok((
        input,
        Spanned {
            node: TypeDesc::Map {
                from: Box::new(from_1),
                to: Box::new(inner_map),
            },
            span: Span::from_pair(map_kw_1, r_bracket),
        },
    ))
}

/// Parse a map type ending with a greater-than token.
///
/// Example:
/// map< address, map<u256, map<bool, int128>> >
pub fn map_type_single(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, map_kw) = name("map")(input)?;
    let (input, _) = op("<")(input)?;
    let (input, from) = base_type(input)?;
    let (input, _) = op(",")(input)?;
    let (input, to) = type_desc(input)?;
    let (input, r_bracket) = op(">")(input)?;

    Ok((
        input,
        Spanned {
            node: TypeDesc::Map {
                from: Box::new(from),
                to: Box::new(to),
            },
            span: Span::from_pair(map_kw, r_bracket),
        },
    ))
}

/// Parse a base type along with an optional array dimension list.
///
/// Example:
/// int128[2][3]
pub fn base_type(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, base) = name_token(input)?;
    let (input, dims) = arr_list(input)?;

    let mut result = Spanned {
        node: TypeDesc::Base { base: base.string },
        span: base.into(),
    };
    for dim in dims {
        let span = Span::from_pair(&result, &dim);

        result = Spanned {
            node: TypeDesc::Array {
                typ: Box::new(result),
                dimension: dim.node,
            },
            span,
        };
    }

    Ok((input, result))
}

/// Parse an array dimension list e.g. "[2][3]"
pub fn arr_list(input: Cursor) -> ParseResult<Vec<Spanned<usize>>> {
    many0(arr_dim)(input)
}

/// Parse an array dimension e.g. "[2]"
pub fn arr_dim(input: Cursor) -> ParseResult<Spanned<usize>> {
    let (num_input, l_bracket) = op("[")(input)?;
    let (input, num_tok) = number_token(num_input)?;
    let (input, r_bracket) = op("]")(input)?;

    let n: usize = match num_tok.string.parse() {
        Ok(n) => n,
        Err(_) => {
            return Err(ParseError::str(
                num_input,
                format!("invalid integer literal \"{}\"", num_tok.string),
            ))
        }
    };

    Ok((
        input,
        Spanned {
            node: n,
            span: Span::from_pair(l_bracket, r_bracket),
        },
    ))
}

pub fn try_from_tok<'a, P, O>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<O>
where
    O: TryFrom<&'a Token<'a>>,
    <O as TryFrom<&'a Token<'a>>>::Error: std::fmt::Debug,
    P: Fn(Cursor<'a>) -> ParseResult<&Token>,
{
    map(parser, |tok| TryFrom::try_from(tok).unwrap())
}

/// Parse a contract field qualifier keyword e.g. "const".
pub fn contract_field_qual(input: Cursor) -> ParseResult<Spanned<ContractFieldQual>> {
    try_from_tok(alt((name("const"), name("pub"))))(input)
}

pub fn contract_field_pub_qual(input: Cursor) -> ParseResult<Spanned<ContractFieldQual>> {
    try_from_tok(name("pub"))(input)
}

pub fn contract_field_const_qual(input: Cursor) -> ParseResult<Spanned<ContractFieldQual>> {
    try_from_tok(name("const"))(input)
}

/// Parse an event field qualifier keyword i.e. "idx".
pub fn event_field_qual(input: Cursor) -> ParseResult<Spanned<EventFieldQual>> {
    try_from_tok(name("idx"))(input)
}

/// Parse a function qualifier keyword i.e. "pub".
pub fn func_qual(input: Cursor) -> ParseResult<Spanned<FuncQual>> {
    try_from_tok(name("pub"))(input)
}

pub fn func_stmt(input: Cursor) -> ParseResult<Vec<Spanned<FuncStmt>>> {
    alt((map(compound_stmt, |stmt| vec![stmt]), simple_stmt))(input)
}

pub fn simple_stmt(input: Cursor) -> ParseResult<Vec<Spanned<FuncStmt>>> {
    terminated(separated(small_stmt, op(";"), true), newline_token)(input)
}

pub fn small_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    alt((
        return_stmt,
        assert_stmt,
        emit_stmt,
        pass_stmt,
        break_stmt,
        continue_stmt,
        revert_stmt,
        vardecl_stmt,
        assign_stmt,
        augassign_stmt,
        map(exprs, |spanned| Spanned {
            node: FuncStmt::Expr {
                value: spanned.node,
            },
            span: spanned.span,
        }),
    ))(input)
}

pub fn return_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    let (input, return_kw) = name("return")(input)?;
    let (input, value) = opt(exprs)(input)?;

    let span = match &value {
        Some(exp) => Span::from_pair(return_kw, exp),
        None => return_kw.span,
    };

    Ok((
        input,
        Spanned {
            node: FuncStmt::Return { value },
            span,
        },
    ))
}

pub fn assert_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    let (input, assert_kw) = name("assert")(input)?;
    let (input, test) = expr(input)?;
    let (input, msg) = opt(preceded(op(","), expr))(input)?;

    let span = match &msg {
        Some(msg_expr) => Span::from_pair(assert_kw, msg_expr),
        None => Span::from_pair(assert_kw, &test),
    };

    Ok((
        input,
        Spanned {
            node: FuncStmt::Assert { test, msg },
            span,
        },
    ))
}

pub fn emit_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    let (input, emit_kw) = name("emit")(input)?;
    let (input, value) = expr(input)?;

    let span = Span::from_pair(emit_kw, &value);

    Ok((
        input,
        Spanned {
            node: FuncStmt::Emit { value },
            span,
        },
    ))
}

pub fn keyword_statement<'a, G>(
    string: &'a str,
    get_stmt: G,
) -> impl Fn(Cursor<'a>) -> ParseResult<Spanned<FuncStmt>>
where
    G: Fn() -> FuncStmt<'a>,
{
    move |input| {
        map(name(string), |t| Spanned {
            node: get_stmt(),
            span: t.span,
        })(input)
    }
}

pub fn pass_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    keyword_statement("pass", || FuncStmt::Pass)(input)
}

pub fn break_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    keyword_statement("break", || FuncStmt::Break)(input)
}

pub fn continue_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    keyword_statement("continue", || FuncStmt::Continue)(input)
}

pub fn revert_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    keyword_statement("revert", || FuncStmt::Revert)(input)
}

pub fn vardecl_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    let (input, target_expr) = target(input)?;
    let (input, _) = op(":")(input)?;
    let (input, typ) = type_desc(input)?;
    let (input, value) = opt(preceded(op("="), expr))(input)?;

    let span = match &value {
        Some(exp) => Span::from_pair(&target_expr, exp),
        None => Span::from_pair(&target_expr, &typ),
    };

    Ok((
        input,
        Spanned {
            node: FuncStmt::VarDecl {
                target: target_expr,
                typ,
                value,
            },
            span,
        },
    ))
}

pub fn assign_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    let (input, targets_vec) = many1(terminated(targets, op("=")))(input)?;
    let (input, value) = exprs(input)?;

    let first = targets_vec.first().unwrap();
    let span = Span::from_pair(first, &value);

    Ok((
        input,
        Spanned {
            node: FuncStmt::Assign {
                targets: targets_vec,
                value,
            },
            span,
        },
    ))
}

pub fn augassign_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    let (input, target_expr) = target(input)?;
    let (input, aug_tok) = alt((
        op("+="),
        op("-="),
        op("*="),
        op("/="),
        op("%="),
        op("&="),
        op("|="),
        op("^="),
        op("<<="),
        op(">>="),
        op("**="),
        op("//="),
    ))(input)?;
    let (input, value) = expr(input)?;

    let span = Span::from_pair(&target_expr, &value);

    Ok((
        input,
        Spanned {
            node: FuncStmt::AugAssign {
                target: target_expr,
                op: TryFrom::try_from(aug_tok).unwrap(),
                value,
            },
            span,
        },
    ))
}

pub fn compound_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    alt((if_stmt, while_stmt, for_stmt))(input)
}

#[allow(clippy::needless_lifetimes)]
pub fn if_stmt_builder<'a>(
    string: &'a str,
) -> impl Fn(Cursor<'a>) -> ParseResult<Spanned<FuncStmt>> {
    move |input| {
        alt((
            |input| {
                let (input, keyword) = name(string)(input)?;
                let (input, test) = expr(input)?;
                let (input, _) = op(":")(input)?;
                let (input, body) = block(input)?;
                let (input, or_else) = elif_stmt(input)?;

                let span = Span::from_pair(keyword, &or_else);
                let or_else = vec![or_else];

                Ok((
                    input,
                    Spanned {
                        node: FuncStmt::If {
                            test,
                            body,
                            or_else,
                        },
                        span,
                    },
                ))
            },
            |input| {
                let (input, keyword) = name(string)(input)?;
                let (input, test) = expr(input)?;
                let (input, _) = op(":")(input)?;
                let (input, body) = block(input)?;
                let (input, or_else) = opt(else_block)(input)?;

                let last_stmt = match &or_else {
                    Some(vec) => vec.last().unwrap(),
                    None => body.last().unwrap(),
                };
                let span = Span::from_pair(keyword, last_stmt);
                let or_else = or_else.unwrap_or_else(Vec::new);

                Ok((
                    input,
                    Spanned {
                        node: FuncStmt::If {
                            test,
                            body,
                            or_else,
                        },
                        span,
                    },
                ))
            },
        ))(input)
    }
}

pub fn if_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    if_stmt_builder("if")(input)
}

pub fn elif_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    if_stmt_builder("elif")(input)
}

pub fn else_block(input: Cursor) -> ParseResult<Vec<Spanned<FuncStmt>>> {
    let (input, _) = name("else")(input)?;
    let (input, _) = op(":")(input)?;
    let (input, stmts) = block(input)?;

    Ok((input, stmts))
}

pub fn while_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    let (input, while_kw) = name("while")(input)?;
    let (input, test) = expr(input)?;
    let (input, _) = op(":")(input)?;
    let (input, body) = block(input)?;
    let (input, or_else) = opt(else_block)(input)?;

    let last_stmt = match &or_else {
        Some(or_else_body) => or_else_body.last().unwrap(),
        None => body.last().unwrap(),
    };
    let span = Span::from_pair(while_kw, last_stmt);
    let or_else = or_else.unwrap_or_else(Vec::new);

    Ok((
        input,
        Spanned {
            node: FuncStmt::While {
                test,
                body,
                or_else,
            },
            span,
        },
    ))
}

pub fn for_stmt(input: Cursor) -> ParseResult<Spanned<FuncStmt>> {
    let (input, for_kw) = name("for")(input)?;
    let (input, target_expr) = targets(input)?;
    let (input, _) = name("in")(input)?;
    let (input, iter) = exprs(input)?;
    let (input, _) = op(":")(input)?;
    let (input, body) = block(input)?;
    let (input, or_else) = opt(else_block)(input)?;

    let last_stmt = match &or_else {
        Some(or_else_body) => or_else_body.last().unwrap(),
        None => body.last().unwrap(),
    };
    let span = Span::from_pair(for_kw, last_stmt);
    let or_else = or_else.unwrap_or_else(Vec::new);

    Ok((
        input,
        Spanned {
            node: FuncStmt::For {
                target: target_expr,
                iter,
                body,
                or_else,
            },
            span,
        },
    ))
}

pub fn block(input: Cursor) -> ParseResult<Vec<Spanned<FuncStmt>>> {
    alt((simple_stmt, |input| {
        let (input, _) = newline_token(input)?;
        let (input, _) = indent_token(input)?;
        let (input, stmts) = many1(func_stmt)(input)?;
        let (input, _) = dedent_token(input)?;

        let result: Vec<_> = stmts.into_iter().flatten().collect();

        Ok((input, result))
    }))(input)
}

/// Parse a comma-separated list of expressions.
pub fn exprs(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let (input, mut elts) = separated(expr, op(","), false)(input)?;
    let (input, comma) = opt(op(","))(input)?;

    let first = elts.first().unwrap();

    let result = match comma {
        Some(comma_tok) => {
            let span = Span::from_pair(first, comma_tok);

            Spanned {
                node: Expr::Tuple { elts },
                span,
            }
        }
        None => {
            if elts.len() > 1 {
                let last = elts.last().unwrap();
                let span = Span::from_pair(first, last);

                Spanned {
                    node: Expr::Tuple { elts },
                    span,
                }
            } else {
                elts.pop().unwrap()
            }
        }
    };

    Ok((input, result))
}

pub fn expr(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let (input, if_expr) = disjunct(input)?;
    let (input, ternary) = opt(|input| {
        let (input, _) = name("if")(input)?;
        let (input, test) = disjunct(input)?;
        let (input, _) = name("else")(input)?;
        let (input, else_expr) = expr(input)?;
        Ok((input, (test, else_expr)))
    })(input)?;

    let result = match ternary {
        Some((test, else_expr)) => {
            let span = Span::from_pair(&if_expr, &else_expr);

            Spanned {
                node: Expr::Ternary {
                    if_expr: Box::new(if_expr),
                    test: Box::new(test),
                    else_expr: Box::new(else_expr),
                },
                span,
            }
        }
        None => if_expr,
    };

    Ok((input, result))
}

#[inline]
pub fn bool_op_builder<'a>(
    left: Spanned<Expr<'a>>,
    op: &'a Token,
    right: Spanned<Expr<'a>>,
) -> Expr<'a> {
    Expr::BoolOperation {
        left: Box::new(left),
        op: TryFrom::try_from(op).unwrap(),
        right: Box::new(right),
    }
}

#[inline]
pub fn bin_op_builder<'a>(
    left: Spanned<Expr<'a>>,
    op: &'a Token,
    right: Spanned<Expr<'a>>,
) -> Expr<'a> {
    Expr::BinOperation {
        left: Box::new(left),
        op: TryFrom::try_from(op).unwrap(),
        right: Box::new(right),
    }
}

#[inline]
pub fn unary_op_builder<'a>(op: &'a Token, operand: Spanned<Expr<'a>>) -> Expr<'a> {
    Expr::UnaryOperation {
        op: TryFrom::try_from(op).unwrap(),
        operand: Box::new(operand),
    }
}

#[inline]
pub fn comp_op_builder<'a>(
    left: Spanned<Expr<'a>>,
    op: Spanned<CompOperator>,
    right: Spanned<Expr<'a>>,
) -> Expr<'a> {
    Expr::CompOperation {
        left: Box::new(left),
        op,
        right: Box::new(right),
    }
}

pub fn disjunct(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(conjunct, name("or"), bool_op_builder)(input)
}

pub fn conjunct(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(comparison, name("and"), bool_op_builder)(input)
}

pub fn comparison(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let (input, nots) = many0(name("not"))(input)?;
    let (input, op_expr) = op_expr_builder(bitwise_or, comp_op, comp_op_builder)(input)?;

    let mut result = op_expr;
    for not_tok in nots.into_iter().rev() {
        let span = Span::from_pair(not_tok, &result);

        result = Spanned {
            node: unary_op_builder(not_tok, result),
            span,
        };
    }

    Ok((input, result))
}

pub fn comp_op(input: Cursor) -> ParseResult<Spanned<CompOperator>> {
    alt((
        map(
            alt((pair(name("not"), name("in")), pair(name("is"), name("not")))),
            |toks| {
                let (fst, snd) = toks;
                TryFrom::try_from(&[fst, snd][..]).unwrap()
            },
        ),
        map(
            alt((
                op("<"),
                op("<="),
                op("=="),
                op(">="),
                op(">"),
                op("!="),
                name("in"),
                name("is"),
            )),
            |tok| TryFrom::try_from(&[tok][..]).unwrap(),
        ),
    ))(input)
}

pub fn bitwise_or(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(bitwise_xor, op("|"), bin_op_builder)(input)
}

pub fn bitwise_xor(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(bitwise_and, op("^"), bin_op_builder)(input)
}

pub fn bitwise_and(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(shift_expr, op("&"), bin_op_builder)(input)
}

pub fn shift_expr(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(sum, alt((op("<<"), op(">>"))), bin_op_builder)(input)
}

pub fn sum(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(term, alt((op("+"), op("-"))), bin_op_builder)(input)
}

pub fn term(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(
        factor,
        alt((op("*"), op("/"), op("//"), op("%"))),
        bin_op_builder,
    )(input)
}

pub fn factor(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let unary_op = |input| {
        let (input, op_tok) = alt((op("+"), op("-"), op("~")))(input)?;
        let (input, factor_expr) = factor(input)?;

        let span = Span::from_pair(op_tok, &factor_expr);

        Ok((
            input,
            Spanned {
                node: unary_op_builder(op_tok, factor_expr),
                span,
            },
        ))
    };

    alt((unary_op, power))(input)
}

pub fn power(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let power_op = |input| {
        let (input, primary_expr) = primary(input)?;
        let (input, op_tok) = op("**")(input)?;
        let (input, factor_expr) = factor(input)?;

        let span = Span::from_pair(&primary_expr, &factor_expr);

        Ok((
            input,
            Spanned {
                node: bin_op_builder(primary_expr, op_tok, factor_expr),
                span,
            },
        ))
    };

    alt((power_op, primary))(input)
}

pub fn build_tail_expr<'a>(exp: Spanned<Expr<'a>>, tails: Vec<Tail<'a>>) -> Spanned<Expr<'a>> {
    let mut result = exp;

    for tail in tails {
        match tail {
            Tail::Attr(name_tok) => {
                let span = Span::from_pair(&result, name_tok);

                result = Spanned {
                    node: Expr::Attribute {
                        value: Box::new(result),
                        attr: name_tok.into(),
                    },
                    span,
                };
            }
            Tail::Index(slices) => {
                let span = Span::from_pair(&result, &slices);

                result = Spanned {
                    node: Expr::Subscript {
                        value: Box::new(result),
                        slices,
                    },
                    span,
                };
            }
            Tail::Call(args) => {
                let span = Span::from_pair(&result, &args);

                result = Spanned {
                    node: Expr::Call {
                        func: Box::new(result),
                        args,
                    },
                    span,
                };
            }
        }
    }

    result
}

pub fn primary(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let (input, atom_expr) = atom(input)?;
    let (input, tails) = many0(alt((attr_tail, index_tail, call_tail)))(input)?;

    Ok((input, build_tail_expr(atom_expr, tails)))
}

pub fn slices(input: Cursor) -> ParseResult<Vec<Spanned<Slice>>> {
    separated(slice, op(","), true)(input)
}

pub fn slice(input: Cursor) -> ParseResult<Spanned<Slice>> {
    alt((
        |input| {
            let boxed_expr = map(expr, Box::new);

            let (input, lower) = opt(&boxed_expr)(input)?;
            let (input, colon) = op(":")(input)?;
            let (input, upper) = opt(&boxed_expr)(input)?;
            let (input, step) = opt(preceded(op(":"), &boxed_expr))(input)?;

            let first = match &lower {
                Some(bx) => bx.span,
                None => colon.span,
            };
            let last = match (&upper, &step) {
                (_, Some(bx)) => bx.span,
                (Some(bx), _) => bx.span,
                _ => colon.span,
            };

            let span = Span::from_pair(first, last);

            Ok((
                input,
                Spanned {
                    node: Slice::Slice { lower, upper, step },
                    span,
                },
            ))
        },
        map(expr, |e| Spanned {
            node: Slice::Index(Box::new(e.node)),
            span: e.span,
        }),
    ))(input)
}

pub fn atom(input: Cursor) -> ParseResult<Spanned<Expr>> {
    alt((
        map(name("true"), |tok| Spanned {
            node: Expr::Bool(true),
            span: tok.span,
        }),
        map(name("false"), |tok| Spanned {
            node: Expr::Bool(false),
            span: tok.span,
        }),
        list,
        map(group, |exp| Spanned {
            node: exp.node.node,
            span: exp.span,
        }),
        tuple,
        map(name_token, |tok| Spanned {
            node: Expr::Name(tok.string),
            span: tok.span,
        }),
        map(number_token, |tok| Spanned {
            node: Expr::Num(tok.string),
            span: tok.span,
        }),
        map(many1(string_token), |toks| {
            let tok_strings: Vec<_> = toks
                .iter()
                .map(|t| {
                    // We don't want to carry quotes around strings past the parsing stage
                    &t.string[1..t.string.len() - 1]
                })
                .collect();

            let fst = toks.first().unwrap();
            let snd = toks.last().unwrap();

            Spanned {
                node: Expr::Str(tok_strings),
                span: Span::from_pair(*fst, *snd),
            }
        }),
        map(op("..."), |tok| Spanned {
            node: Expr::Ellipsis,
            span: tok.span,
        }),
    ))(input)
}

pub fn list(input: Cursor) -> ParseResult<Spanned<Expr>> {
    map(delimited(op("["), opt(exprs), op("]")), |spanned| {
        use Expr::{
            List,
            Tuple,
        };

        let node = match spanned.node {
            Some(Spanned {
                node: Tuple { elts },
                ..
            }) => List { elts },
            Some(exp) => List { elts: vec![exp] },
            None => List { elts: vec![] },
        };
        let span = spanned.span;

        Spanned { node, span }
    })(input)
}

pub fn tuple(input: Cursor) -> ParseResult<Spanned<Expr>> {
    map(delimited(op("("), opt(exprs), op(")")), |spanned| {
        use Expr::Tuple;

        let node = match spanned.node {
            Some(Spanned {
                node: Tuple { elts },
                ..
            }) => Tuple { elts },
            Some(exp) => exp.node,
            None => Tuple { elts: vec![] },
        };
        let span = spanned.span;

        Spanned { node, span }
    })(input)
}

pub fn group(input: Cursor) -> ParseResult<Spanned<Spanned<Expr>>> {
    delimited(op("("), expr, op(")"))(input)
}

pub fn args(input: Cursor) -> ParseResult<Vec<Spanned<CallArg>>> {
    let kw_result = kwargs(input);
    if kw_result.is_ok() {
        return kw_result;
    }

    let (input, first) = expr(input)?;
    let (input, rest) = opt(preceded(op(","), args))(input)?;

    let mut results = vec![Spanned {
        node: CallArg::Arg(first.node),
        span: first.span,
    }];
    if let Some(mut rest) = rest {
        results.append(&mut rest);
    }

    Ok((input, results))
}

pub fn kwargs(input: Cursor) -> ParseResult<Vec<Spanned<CallArg>>> {
    separated(kwarg, op(","), false)(input)
}

pub fn kwarg(input: Cursor) -> ParseResult<Spanned<CallArg>> {
    let (input, name_tok) = name_token(input)?;
    let (input, _) = op("=")(input)?;
    let (input, value_expr) = expr(input)?;

    let span = Span::from_pair(name_tok, &value_expr);

    Ok((
        input,
        Spanned {
            node: CallArg::Kwarg(Kwarg {
                name: name_tok.into(),
                value: Box::new(value_expr),
            }),
            span,
        },
    ))
}

pub enum Tail<'a> {
    Attr(&'a Token<'a>),
    Index(Spanned<Vec<Spanned<Slice<'a>>>>),
    Call(Spanned<Vec<Spanned<CallArg<'a>>>>),
}

pub fn targets(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let (input, mut elts) = separated(target, op(","), false)(input)?;
    let (input, comma) = opt(op(","))(input)?;

    let first = elts.first().unwrap();

    let result = match comma {
        Some(comma_tok) => {
            let span = Span::from_pair(first, comma_tok);

            Spanned {
                node: Expr::Tuple { elts },
                span,
            }
        }
        None => {
            if elts.len() > 1 {
                let last = elts.last().unwrap();
                let span = Span::from_pair(first, last);

                Spanned {
                    node: Expr::Tuple { elts },
                    span,
                }
            } else {
                elts.pop().unwrap()
            }
        }
    };

    Ok((input, result))
}

pub fn target(input: Cursor) -> ParseResult<Spanned<Expr>> {
    alt((
        |input| {
            let (input, atom_expr) = atom(input)?;
            let (input, tails) = many1(t_tail)(input)?;

            let tails: Vec<_> = tails.into_iter().flatten().collect();

            Ok((input, build_tail_expr(atom_expr, tails)))
        },
        |input| {
            let (input, atom_expr) = t_atom(input)?;
            let (input, tails) = many0(t_tail)(input)?;

            let tails: Vec<_> = tails.into_iter().flatten().collect();

            Ok((input, build_tail_expr(atom_expr, tails)))
        },
    ))(input)
}

pub fn t_atom(input: Cursor) -> ParseResult<Spanned<Expr>> {
    alt((
        map(name_token, |tok| Spanned {
            node: Expr::Name(tok.string),
            span: tok.span,
        }),
        map(delimited(op("("), targets, op(")")), |spanned| {
            use Expr::Tuple;

            let node = match spanned.node {
                Spanned {
                    node: Tuple { elts },
                    ..
                } => Tuple { elts },
                exp => Tuple { elts: vec![exp] },
            };
            let span = spanned.span;

            Spanned { node, span }
        }),
        map(delimited(op("["), targets, op("]")), |spanned| {
            use Expr::{
                List,
                Tuple,
            };

            let node = match spanned.node {
                Spanned {
                    node: Tuple { elts },
                    ..
                } => List { elts },
                exp => List { elts: vec![exp] },
            };
            let span = spanned.span;

            Spanned { node, span }
        }),
    ))(input)
}

pub fn t_tail(input: Cursor) -> ParseResult<Vec<Tail>> {
    let (input, mut tails) = many0(call_tail)(input)?;
    let (input, last) = alt((attr_tail, index_tail))(input)?;

    tails.push(last);

    Ok((input, tails))
}

pub fn attr_tail(input: Cursor) -> ParseResult<Tail> {
    map(preceded(op("."), name_token), Tail::Attr)(input)
}

pub fn index_tail(input: Cursor) -> ParseResult<Tail> {
    map(delimited(op("["), slices, op("]")), Tail::Index)(input)
}

pub fn call_tail(input: Cursor) -> ParseResult<Tail> {
    map(
        map(delimited(op("("), opt(args), op(")")), |spanned| Spanned {
            node: spanned.node.unwrap_or_else(Vec::new),
            span: spanned.span,
        }),
        Tail::Call,
    )(input)
}
