use anyhow::bail;
use clap::Parser;
use pretty_hex::pretty_hex;
use std::{
    collections::HashMap, fmt::Display, fs, iter::Peekable, path::PathBuf, str::CharIndices,
};

#[derive(Debug)]
enum Stmt {
    DotEntry(Location),
    DotPos(u64),
    DotAlign(u64),
    DotQuad(u64),
    Label(String),
    Halt,
    Nop,
    Rrmovq(Reg, Reg),
    Irmovq(Location, Reg),
    Rmmovq(Reg, MemAccess),
    Mrmovq(MemAccess, Reg),
    Addq(Reg, Reg),
    Subq(Reg, Reg),
    Andq(Reg, Reg),
    Xorq(Reg, Reg),
    Jmp(Location),
    Jle(Location),
    Jl(Location),
    Je(Location),
    Jne(Location),
    Jge(Location),
    Jg(Location),
    Cmovle(Reg, Reg),
    Cmovl(Reg, Reg),
    Cmove(Reg, Reg),
    Cmovne(Reg, Reg),
    Cmovge(Reg, Reg),
    Cmovg(Reg, Reg),
    Call(Location),
    Ret,
    Pushq(Reg),
    Popq(Reg),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Stmt::*;
        match self {
            DotEntry(loc) => write!(f, ".entry {}", loc),
            DotPos(pos) => write!(f, ".pos {:x}", pos),
            DotAlign(align) => write!(f, ".align 0x{:x}", align),
            DotQuad(quad) => write!(f, ".quad 0x{:x}", quad),
            Label(lab) => write!(f, "{}", lab),
            Halt => write!(f, "halt"),
            Nop => write!(f, "nop"),
            Rrmovq(ra, rb) => write!(f, "rrmovq {}, {}", ra, rb),
            Irmovq(loc, r) => write!(f, "irmoveq {}, {}", loc, r),
            Rmmovq(r, ma) => write!(f, "rmmovq {}, {}", r, ma),
            Mrmovq(ma, r) => write!(f, "mrmovq {}, {}", ma, r),
            Addq(ra, rb) => write!(f, "addq {}, {}", ra, rb),
            Subq(ra, rb) => write!(f, "subq {}, {}", ra, rb),
            Andq(ra, rb) => write!(f, "andq {}, {}", ra, rb),
            Xorq(ra, rb) => write!(f, "xorq {}, {}", ra, rb),
            Jmp(loc) => write!(f, "jmp {}", loc),
            Jle(loc) => write!(f, "jle {}", loc),
            Jl(loc) => write!(f, "jl {}", loc),
            Je(loc) => write!(f, "je {}", loc),
            Jne(loc) => write!(f, "jne {}", loc),
            Jge(loc) => write!(f, "jge {}", loc),
            Jg(loc) => write!(f, "jg {}", loc),
            Cmovle(ra, rb) => write!(f, "cmovle {}, {}", ra, rb),
            Cmovl(ra, rb) => write!(f, "cmovl {}, {}", ra, rb),
            Cmove(ra, rb) => write!(f, "cmove {}, {}", ra, rb),
            Cmovne(ra, rb) => write!(f, "cmovne {}, {}", ra, rb),
            Cmovge(ra, rb) => write!(f, "cmovge {}, {}", ra, rb),
            Cmovg(ra, rb) => write!(f, "cmovg {}, {}", ra, rb),
            Call(loc) => write!(f, "call {}", loc),
            Ret => write!(f, "ret"),
            Pushq(r) => write!(f, "pushq {}", r),
            Popq(r) => write!(f, "popq {}", r),
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Abs(abs) => write!(f, "0x{:x}", abs),
            Location::Label(lab) => write!(f, "{}", lab),
        }
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Reg::*;
        match self {
            Rax => write!(f, "%rax"),
            Rcx => write!(f, "%rcx"),
            Rdx => write!(f, "%rdx"),
            Rbx => write!(f, "%rbx"),
            Rsp => write!(f, "%rsp"),
            Rbp => write!(f, "%rbp"),
            Rsi => write!(f, "%rsi"),
            Rdi => write!(f, "%rdi"),
            R8 => write!(f, "%r8"),
            R9 => write!(f, "%r9"),
            R10 => write!(f, "%r10"),
            R11 => write!(f, "%r11"),
            R12 => write!(f, "%r12"),
            R13 => write!(f, "%r13"),
            R14 => write!(f, "%r14"),
        }
    }
}

impl Display for MemAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.offset == 0 {
            write!(f, "({})", self.reg)
        } else {
            write!(f, "0x{:x}({})", self.offset, self.reg)
        }
    }
}

enum OpWord {
    Keyword(String),
    Label(String),
    Other(String),
}

#[derive(Debug)]
enum Location {
    Label(String),
    Abs(u64),
}

#[derive(Debug)]
struct MemAccess {
    offset: i64,
    reg: Reg,
}

#[derive(Debug)]
enum Reg {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
}

struct Asm<'a> {
    cursor: Peekable<CharIndices<'a>>,
    ln: usize,
}

impl<'a> Asm<'a> {
    pub fn new(input: &'a str) -> Self {
        Asm {
            cursor: input.char_indices().peekable(),
            ln: 1,
        }
    }

    pub fn parse(&mut self) -> anyhow::Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while let Some((i, c)) = self.opt_space() {
            match c {
                '#' | '\n' => {}
                _ => match self.expect_opword()? {
                    OpWord::Label(lab) => {
                        stmts.push(Stmt::Label(lab));
                    }
                    OpWord::Keyword(kw) => match kw.as_str() {
                        ".pos" => {
                            self.expect_space()?;
                            stmts.push(Stmt::DotPos(self.expect_u64()?));
                        }
                        ".quad" => {
                            self.expect_space()?;
                            stmts.push(Stmt::DotQuad(self.expect_u64()?));
                        }
                        ".align" => {
                            self.expect_space()?;
                            stmts.push(Stmt::DotAlign(self.expect_u64()?));
                        }
                        ".entry" => {
                            self.expect_space()?;
                            stmts.push(Stmt::DotEntry(self.expect_location()?));
                        }
                        _ => bail!("invalid key word {},{}:{}", self.ln, i, kw),
                    },
                    OpWord::Other(op) => match op.as_str() {
                        "halt" => stmts.push(Stmt::Halt),
                        "nop" => stmts.push(Stmt::Nop),
                        "rrmovq" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Rrmovq(ra, rb));
                        }
                        "irmovq" => {
                            self.expect_space()?;
                            let loc = self.expect_location()?;
                            self.opt_space();
                            self.expect_char(',')?;
                            self.opt_space();
                            let ra = self.expect_reg()?;
                            stmts.push(Stmt::Irmovq(loc, ra));
                        }
                        "rmmovq" => {
                            self.expect_space()?;
                            let ra = self.expect_reg()?;
                            self.opt_space();
                            self.expect_char(',')?;
                            self.opt_space();
                            let mem = self.expect_mem()?;
                            stmts.push(Stmt::Rmmovq(ra, mem));
                        }
                        "mrmovq" => {
                            self.expect_space()?;
                            let mem = self.expect_mem()?;
                            self.opt_space();
                            self.expect_char(',')?;
                            self.opt_space();
                            let ra = self.expect_reg()?;
                            stmts.push(Stmt::Mrmovq(mem, ra));
                        }
                        "addq" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Addq(ra, rb));
                        }
                        "subq" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Subq(ra, rb));
                        }
                        "andq" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Andq(ra, rb));
                        }
                        "xorq" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Xorq(ra, rb));
                        }
                        "jmp" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Jmp(self.expect_location()?));
                        }
                        "jle" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Jle(self.expect_location()?));
                        }
                        "jl" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Jl(self.expect_location()?));
                        }
                        "je" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Je(self.expect_location()?));
                        }
                        "jne" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Jne(self.expect_location()?));
                        }
                        "jge" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Jge(self.expect_location()?));
                        }
                        "jg" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Jg(self.expect_location()?));
                        }
                        "cmovle" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Cmovle(ra, rb));
                        }
                        "cmovl" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Cmovl(ra, rb));
                        }
                        "cmove" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Cmove(ra, rb));
                        }
                        "cmovne" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Cmovne(ra, rb));
                        }
                        "cmovge" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Cmovge(ra, rb));
                        }
                        "cmovg" => {
                            self.expect_space()?;
                            let (ra, rb) = self.expect_rr()?;
                            stmts.push(Stmt::Cmovg(ra, rb));
                        }
                        "call" => {
                            self.expect_space()?;
                            let loc = self.expect_location()?;
                            stmts.push(Stmt::Call(loc));
                        }
                        "ret" => {
                            stmts.push(Stmt::Ret);
                        }
                        "pushq" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Pushq(self.expect_reg()?));
                        }
                        "popq" => {
                            self.expect_space()?;
                            stmts.push(Stmt::Popq(self.expect_reg()?));
                        }
                        _ => bail!("invalid opword found {},{}:{}", self.ln, i, op.as_str()),
                    },
                },
            }
            self.eat_ln()?;
        }
        Ok(stmts)
    }

    pub fn expect_rr(&mut self) -> anyhow::Result<(Reg, Reg)> {
        let ra = self.expect_reg()?;
        self.opt_space();
        self.expect_char(',')?;
        self.opt_space();
        let rb = self.expect_reg()?;
        Ok((ra, rb))
    }

    pub fn expect_mem(&mut self) -> anyhow::Result<MemAccess> {
        if let Some((i, c)) = self.peek() {
            match c {
                '-' | '+' | '0'..='9' => {
                    let offset = self.expect_i64()?;
                    self.expect_char('(')?;
                    self.opt_space();
                    let reg = self.expect_reg()?;
                    self.opt_space();
                    self.expect_char(')')?;
                    Ok(MemAccess { offset, reg })
                }
                '(' => {
                    self.next();
                    let offset = 0;
                    self.opt_space();
                    let reg = self.expect_reg()?;
                    self.opt_space();
                    self.expect_char(')')?;
                    Ok(MemAccess { offset, reg })
                }
                _ => bail!("invalid mem access pattern {},{}:{}", self.ln, i, c),
            }
        } else {
            bail!("expect mem access pattern but actual EOI {}:", self.ln);
        }
    }

    pub fn expect_char(&mut self, ec: char) -> anyhow::Result<()> {
        if let Some((i, c)) = self.next() {
            if ec == c {
                Ok(())
            } else {
                bail!("expect {} but actual {},{}:{}", ec, self.ln, i, c);
            }
        } else {
            bail!("expect {} but actual EOI {}:", ec, self.ln);
        }
    }

    pub fn expect_reg(&mut self) -> anyhow::Result<Reg> {
        self.expect_char('%')?;
        self.expect_char('r')?;
        if let Some((i, c)) = self.next() {
            let reg = match c {
                'a' => {
                    self.expect_char('x')?;
                    Reg::Rax
                }
                'c' => {
                    self.expect_char('x')?;
                    Reg::Rcx
                }
                'd' => {
                    if let Some((ni, nc)) = self.next() {
                        match nc {
                            'x' => Reg::Rdx,
                            'i' => Reg::Rdi,
                            _ => bail!(
                                "invalid register identifier found {},{}:{}",
                                self.ln,
                                ni,
                                nc
                            ),
                        }
                    } else {
                        bail!(
                            "expect register identifier but actual EOI {}:{}(EOI)",
                            self.ln,
                            c
                        );
                    }
                }
                'b' => {
                    if let Some((ni, nc)) = self.next() {
                        match nc {
                            'x' => Reg::Rbx,
                            'p' => Reg::Rbp,
                            _ => bail!("invalid register identifier {},{}:{}", self.ln, ni, nc),
                        }
                    } else {
                        bail!(
                            "expect register identifier but EOI reached after {}:{}(EOI)",
                            self.ln,
                            c
                        );
                    }
                }
                's' => {
                    if let Some((ni, nc)) = self.next() {
                        match nc {
                            'i' => Reg::Rsi,
                            'p' => Reg::Rsp,
                            _ => bail!("invalid register identifier {},{}:{}", self.ln, ni, nc),
                        }
                    } else {
                        bail!(
                            "expect register identifier but actual EOI {}:{}(EOI)",
                            self.ln,
                            c
                        );
                    }
                }
                '8' => Reg::R8,
                '9' => Reg::R9,
                '1' => {
                    if let Some((ni, nc)) = self.next() {
                        match nc {
                            '0' => Reg::R10,
                            '1' => Reg::R11,
                            '2' => Reg::R12,
                            '3' => Reg::R13,
                            '4' => Reg::R14,
                            _ => bail!("expect R1x register but actual {},{}:{}", self.ln, ni, nc),
                        }
                    } else {
                        bail!("expect R1x register but actual EOI {}:", self.ln);
                    }
                }
                _ => bail!("invalid register identifier {},{}:{}", self.ln, i, c),
            };
            Ok(reg)
        } else {
            bail!("expect register identifier but actual EOI {}:", self.ln);
        }
    }

    pub fn expect_u64(&mut self) -> anyhow::Result<u64> {
        let mut num_str = String::new();
        let mut radix: u32 = 10;
        while let Some(c) = self.peek_char() {
            if radix == 10 {
                match c {
                    '0'..='9' => {
                        num_str.push(c);
                        self.next();
                    }
                    'x' | 'X' => {
                        if num_str == "0" {
                            radix = 16;
                            num_str.clear();
                            self.next();
                        } else {
                            break;
                        }
                    }
                    _ => break,
                }
            } else if radix == 16 {
                match c {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        num_str.push(c);
                        self.next();
                    }
                    _ => break,
                }
            }
        }
        Ok(u64::from_str_radix(num_str.as_str(), radix)?)
    }

    pub fn expect_i64(&mut self) -> anyhow::Result<i64> {
        if let Some((i, c)) = self.peek() {
            match c {
                '-' => {
                    self.next();
                    Ok(-(self.expect_u64()? as i64))
                }
                '+' => {
                    self.next();
                    Ok(self.expect_u64()? as i64)
                }
                '0'..='9' => Ok(self.expect_u64()? as i64),
                _ => bail!("invalid i64 number {},{}:{}", self.ln, i, c),
            }
        } else {
            bail!("expected i64 but actual EOI {}:", self.ln);
        }
    }

    pub fn expect_opword(&mut self) -> anyhow::Result<OpWord> {
        let mut op_str = String::new();
        while let Some((i, c)) = self.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '_' => {
                    self.next();
                    op_str.push(c);
                }
                '0'..='9' => {
                    self.next();
                    if op_str.is_empty() {
                        bail!(
                            "invalid opword: start with number is not allowed {},{}:{}",
                            self.ln,
                            i,
                            c
                        );
                    } else {
                        op_str.push(c);
                    }
                }
                '.' => {
                    self.next();
                    if !op_str.is_empty() {
                        bail!(
                            ".(Dot) not allowed in the mid of a operator {},{}:{}",
                            self.ln,
                            i,
                            c
                        );
                    } else {
                        op_str.push(c);
                    }
                }
                ':' => {
                    self.next();
                    if op_str.is_empty() {
                        bail!("empty label is not allowed {},{}:{}", self.ln, i, c);
                    } else if op_str.starts_with('.') {
                        bail!(
                            "consider remove the leading . for the label {},{}:{}",
                            self.ln,
                            i,
                            op_str
                        );
                    }
                    return Ok(OpWord::Label(op_str));
                }
                _ => break,
            }
        }
        if op_str.is_empty() {
            bail!("missing opword {}:", self.ln);
        } else if op_str.starts_with('.') {
            Ok(OpWord::Keyword(op_str))
        } else {
            Ok(OpWord::Other(op_str))
        }
    }

    pub fn expect_location(&mut self) -> anyhow::Result<Location> {
        if let Some((i, c)) = self.peek() {
            if c == '$' {
                self.next();
                return Ok(Location::Abs(self.expect_u64()?));
            } else {
                let mut loc_str = String::new();
                while let Some(c) = self.peek_char() {
                    match c {
                        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => loc_str.push(c),
                        _ => break,
                    }
                    self.next();
                }
                if loc_str.is_empty() {
                    bail!("missing location {},{}:{}", self.ln, i, c);
                } else {
                    return Ok(Location::Label(loc_str));
                }
            }
        }
        bail!("expect location but actual EOI {}:", self.ln);
    }

    pub fn opt_while<F: Fn(char) -> bool>(&mut self, f: F) -> Option<(usize, char)> {
        while let Some((i, c)) = self.peek() {
            if !f(c) {
                return Some((i, c));
            } else {
                self.next();
            }
        }
        None
    }

    fn eat_ln(&mut self) -> anyhow::Result<()> {
        if let Some((i, c)) = self.opt_space() {
            match c {
                '#' => {
                    self.next();
                    self.eat_comment();
                    return Ok(());
                }
                '\n' => {
                    self.next();
                    self.nx_ln();
                    return Ok(());
                }
                _ => bail!("invalid rest of the line {},{}:{}", self.ln, i, c),
            }
        }
        Ok(())
    }

    fn nx_ln(&mut self) {
        self.ln += 1;
    }

    fn eat_comment(&mut self) {
        if self.opt_while(|c| c != '\n').is_some() {
            self.nx_ln();
            self.next();
        }
    }

    fn expect_space(&mut self) -> anyhow::Result<Option<(usize, char)>> {
        if let Some((i, c)) = self.peek() {
            if c != ' ' {
                bail!("expect space but actual {},{}:{}", self.ln, i, c);
            }
            self.next();
        }
        while let Some((i, c)) = self.peek() {
            if c != ' ' {
                return Ok(Some((i, c)));
            }
            self.next();
        }
        Ok(None)
    }

    fn opt_space(&mut self) -> Option<(usize, char)> {
        while let Some((i, c)) = self.peek() {
            if c == ' ' {
                self.next();
            } else {
                return Some((i, c));
            }
        }
        None
    }

    fn next(&mut self) -> Option<(usize, char)> {
        self.cursor.next()
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.cursor.peek().copied()
    }

    fn peek_char(&mut self) -> Option<char> {
        self.peek().map(|(_, c)| c)
    }
}

#[derive(Debug)]
struct Segment {
    addr: u64,
    binary: Vec<u8>,
}

#[derive(Debug)]
struct Object {
    entry: u64,
    segments: Vec<Segment>,
}

impl Object {}

impl Segment {
    fn new() -> Self {
        Segment {
            addr: 0,
            binary: Vec::new(),
        }
    }

    fn new_at(start: u64) -> Self {
        Segment {
            addr: start,
            binary: Vec::new(),
        }
    }

    fn pos(&self) -> u64 {
        self.addr + self.len()
    }

    fn len(&self) -> u64 {
        self.binary.len() as u64
    }

    fn push_slice(&mut self, inst: &[u8]) {
        self.binary.extend_from_slice(inst);
    }

    fn push_byte(&mut self, byte: u8) {
        self.binary.push(byte);
    }

    fn align(&mut self, align: u64) {
        let cur_pos = self.pos();
        let mask = align - 1;
        let gap = ((cur_pos + mask) & !mask) - cur_pos;
        if gap != 0 {
            self.push_slice(&vec![0; gap as usize]);
        }
    }

    fn overwrite(&mut self, pos: u64, le_bytes: &[u8]) {
        let offset = pos - self.addr;
        for (i, b) in le_bytes.iter().enumerate() {
            self.binary[offset as usize + i] = *b;
        }
    }
}

struct Codegen<'a> {
    stmts: &'a [Stmt],
    segments: Vec<Segment>,
}

impl<'a> Codegen<'a> {
    fn new(stmts: &'a [Stmt]) -> Self {
        Codegen {
            stmts,
            segments: vec![Segment::new()],
        }
    }

    fn segment_id(&self) -> usize {
        self.segments.len() - 1
    }

    fn pos(&self) -> u64 {
        self.segment().pos()
    }

    fn segment(&self) -> &Segment {
        self.segments.last().unwrap()
    }

    fn align(&mut self, align: u64) {
        self.segment_mut().align(align);
    }

    fn segment_mut(&mut self) -> &mut Segment {
        self.segments.last_mut().unwrap()
    }

    fn push_byte(&mut self, byte: u8) {
        self.segment_mut().push_byte(byte);
    }

    fn push_slice(&mut self, slice: &[u8]) {
        self.segment_mut().push_slice(slice);
    }

    fn push_i64(&mut self, n: i64) {
        let le_bytes: [u8; 8] = n.to_le_bytes();
        self.push_slice(&le_bytes);
    }

    fn push_u64(&mut self, n: u64) {
        let le_bytes: [u8; 8] = n.to_le_bytes();
        self.push_slice(&le_bytes);
    }

    fn compile(mut self) -> anyhow::Result<Object> {
        let mut label_map = HashMap::new();
        let mut pending = Vec::new();
        let mut entry = None;

        use Stmt::*;
        for (ln, stmt) in self.stmts.iter().enumerate() {
            match stmt {
                Label(label) => {
                    if label_map.contains_key(label) {
                        bail!("duplicated label {}:{}", ln, label);
                    }
                    label_map.insert(label, self.pos());
                }
                DotAlign(align) => {
                    self.align(*align);
                }
                DotPos(pos) => {
                    if self.segment().addr != *pos {
                        self.segments.push(Segment::new_at(*pos));
                    }
                }
                DotEntry(ent) => {
                    entry = Some(ent);
                }
                DotQuad(data) => {
                    self.push_u64(*data << 8 | 0xFF);
                }
                Halt => self.push_byte(0x00),
                Nop => self.push_byte(0x10),
                Rrmovq(ra, rb) => {
                    self.push_slice(&[0x20, self.compile_rr(ra, rb)]);
                }
                Irmovq(loc, rb) => {
                    let c_reg_byte = 0xF0 | self.compile_reg(rb);
                    self.push_slice(&[0x30, c_reg_byte]);
                    let instant_val = match loc {
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                        Location::Abs(abs_val) => *abs_val,
                    };
                    self.push_u64(instant_val);
                }
                Rmmovq(ra, ma) => {
                    let c_offset: [u8; 8] = ma.offset.to_le_bytes();
                    self.push_slice(&[0x40, self.compile_rr(ra, &ma.reg)]);
                    self.push_slice(&c_offset);
                }
                Mrmovq(ma, ra) => {
                    self.push_slice(&[0x50, self.compile_rr(ra, &ma.reg)]);
                    self.push_i64(ma.offset);
                }
                Addq(ra, rb) => {
                    self.push_slice(&[0x60, self.compile_rr(ra, rb)]);
                }
                Subq(ra, rb) => {
                    self.push_slice(&[0x61, self.compile_rr(ra, rb)]);
                }
                Andq(ra, rb) => {
                    self.push_slice(&[0x62, self.compile_rr(ra, rb)]);
                }
                Xorq(ra, rb) => {
                    self.push_slice(&[0x63, self.compile_rr(ra, rb)]);
                }
                Jmp(loc) => {
                    self.push_byte(0x70);
                    let address = match loc {
                        Location::Abs(abs_addr) => *abs_addr,
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                    };
                    self.push_u64(address);
                }
                Jle(loc) => {
                    self.push_byte(0x71);
                    let address = match loc {
                        Location::Abs(abs_addr) => *abs_addr,
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                    };
                    self.push_u64(address);
                }
                Jl(loc) => {
                    self.push_byte(0x72);
                    let address = match loc {
                        Location::Abs(abs_addr) => *abs_addr,
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                    };
                    self.push_u64(address);
                }
                Je(loc) => {
                    self.push_byte(0x73);
                    let address = match loc {
                        Location::Abs(abs_addr) => *abs_addr,
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                    };
                    self.push_u64(address);
                }
                Jne(loc) => {
                    self.push_byte(0x74);
                    let address = match loc {
                        Location::Abs(abs_addr) => *abs_addr,
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                    };
                    self.push_u64(address);
                }
                Jge(loc) => {
                    self.push_byte(0x75);
                    let address = match loc {
                        Location::Abs(abs_addr) => *abs_addr,
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                    };
                    self.push_u64(address);
                }
                Jg(loc) => {
                    self.push_byte(0x76);
                    let address = match loc {
                        Location::Abs(abs_addr) => *abs_addr,
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                    };
                    self.push_u64(address);
                }
                Cmovle(ra, rb) => {
                    self.push_slice(&[0x21, self.compile_rr(ra, rb)]);
                }
                Cmovl(ra, rb) => {
                    self.push_slice(&[0x22, self.compile_rr(ra, rb)]);
                }
                Cmove(ra, rb) => {
                    self.push_slice(&[0x23, self.compile_rr(ra, rb)]);
                }
                Cmovne(ra, rb) => {
                    self.push_slice(&[0x24, self.compile_rr(ra, rb)]);
                }
                Cmovge(ra, rb) => {
                    self.push_slice(&[0x25, self.compile_rr(ra, rb)]);
                }
                Cmovg(ra, rb) => {
                    self.push_slice(&[0x26, self.compile_rr(ra, rb)]);
                }
                Call(loc) => {
                    self.push_byte(0x80);
                    let address = match loc {
                        Location::Abs(abs_addr) => *abs_addr,
                        Location::Label(label) => {
                            if label_map.contains_key(label) {
                                *label_map.get(label).unwrap()
                            } else {
                                pending.push((self.segment_id(), self.pos(), label));
                                0
                            }
                        }
                    };
                    self.push_u64(address);
                }
                Ret => {
                    self.push_byte(0x90);
                }
                Pushq(reg) => {
                    let c_reg = self.compile_reg(reg) << 4 | 0x0F;
                    self.push_slice(&[0xA0, c_reg]);
                }
                Popq(reg) => {
                    let c_reg = self.compile_reg(reg) << 4 | 0x0F;
                    self.push_slice(&[0xB0, c_reg]);
                }
            }
        }

        for (sid, pos, label) in pending {
            if label_map.contains_key(label) {
                let address_bytes: [u8; 8] = label_map.get(label).unwrap().to_le_bytes();
                self.segments[sid].overwrite(pos, &address_bytes);
            } else {
                bail!("label not defined {}", label);
            }
        }

        let mut entry_addr = 0;
        if let Some(ent) = entry {
            entry_addr = match ent {
                Location::Abs(abs_addr) => *abs_addr,
                Location::Label(label) => {
                    if label_map.contains_key(label) {
                        *label_map.get(label).unwrap()
                    } else {
                        bail!("label not defined {}", label);
                    }
                }
            }
        }
        Ok(Object {
            segments: self.segments,
            entry: entry_addr,
        })
    }

    fn compile_rr(&self, ra: &Reg, rb: &Reg) -> u8 {
        self.compile_reg(ra) << 4 | self.compile_reg(rb)
    }

    fn compile_reg(&self, reg: &Reg) -> u8 {
        use Reg::*;
        match reg {
            Rax => 0,
            Rcx => 1,
            Rdx => 2,
            Rbx => 3,
            Rsp => 4,
            Rbp => 5,
            Rsi => 6,
            Rdi => 7,
            R8 => 8,
            R9 => 9,
            R10 => 10,
            R11 => 11,
            R12 => 12,
            R13 => 13,
            R14 => 14,
        }
    }
}

struct Disassembler<'a> {
    object: &'a Object,
    label_cnt: usize,
}

impl<'a> Disassembler<'a> {
    fn new(object: &'a Object) -> Self {
        Disassembler {
            object,
            label_cnt: 1,
        }
    }

    fn disassemble(&mut self) -> anyhow::Result<Vec<(u64, Stmt)>> {
        let mut cmd_vec = Vec::new();
        let entry = self.object.entry;
        cmd_vec.push((0, Stmt::DotEntry(Location::Abs(entry))));
        for segment in &self.object.segments {
            let mut addr = segment.addr;
            cmd_vec.push((addr, Stmt::Label(self.gen_label())));
            cmd_vec.push((addr, Stmt::DotPos(addr)));

            let mut bf = ByteBuf::new(&segment.binary);
            while let Some(bii) = bf.next_u8() {
                let (stmt, len) = match bii {
                    0x00 => (Stmt::Halt, 1),
                    0x10 => (Stmt::Nop, 1),
                    0x20 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Rrmovq(ra, rb), 2)
                    }
                    0x30 => {
                        let f_rbi = bf.expect_u8()?;
                        let rbi = f_rbi & 0xF;
                        let rb = self.decompile_reg(rbi)?;
                        let iv = bf.expect_u64()?;
                        (Stmt::Irmovq(Location::Abs(iv), rb), 10)
                    }
                    0x40 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        let iv = bf.expect_i64()?;
                        (
                            Stmt::Rmmovq(
                                ra,
                                MemAccess {
                                    offset: iv,
                                    reg: rb,
                                },
                            ),
                            10,
                        )
                    }
                    0x50 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        let iv = bf.expect_i64()?;
                        (
                            Stmt::Mrmovq(
                                MemAccess {
                                    offset: iv,
                                    reg: rb,
                                },
                                ra,
                            ),
                            10,
                        )
                    }
                    0x60 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Addq(ra, rb), 2)
                    }
                    0x61 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Subq(ra, rb), 2)
                    }
                    0x62 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Andq(ra, rb), 2)
                    }
                    0x63 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Xorq(ra, rb), 2)
                    }
                    0x70 => {
                        let jaddr = bf.expect_u64()?;
                        (Stmt::Jmp(Location::Abs(jaddr)), 9)
                    }
                    0x71 => {
                        let jaddr = bf.expect_u64()?;
                        (Stmt::Jle(Location::Abs(jaddr)), 9)
                    }
                    0x72 => {
                        let jaddr = bf.expect_u64()?;
                        (Stmt::Jl(Location::Abs(jaddr)), 9)
                    }
                    0x73 => {
                        let jaddr = bf.expect_u64()?;
                        (Stmt::Je(Location::Abs(jaddr)), 9)
                    }
                    0x74 => {
                        let jaddr = bf.expect_u64()?;
                        (Stmt::Jne(Location::Abs(jaddr)), 9)
                    }
                    0x75 => {
                        let jaddr = bf.expect_u64()?;
                        (Stmt::Jge(Location::Abs(jaddr)), 9)
                    }
                    0x76 => {
                        let jaddr = bf.expect_u64()?;
                        (Stmt::Jg(Location::Abs(jaddr)), 9)
                    }
                    0x21 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Cmovle(ra, rb), 2)
                    }
                    0x22 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Cmovl(ra, rb), 2)
                    }
                    0x23 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Cmove(ra, rb), 2)
                    }
                    0x24 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Cmovne(ra, rb), 2)
                    }
                    0x25 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Cmovge(ra, rb), 2)
                    }
                    0x26 => {
                        let (ra, rb) = self.decompile_rr(bf.expect_u8()?)?;
                        (Stmt::Cmovg(ra, rb), 2)
                    }
                    0x80 => {
                        let caddr = bf.expect_u64()?;
                        (Stmt::Call(Location::Abs(caddr)), 9)
                    }
                    0x90 => (Stmt::Ret, 1),
                    0xA0 => {
                        let rafi = bf.expect_u8()?;
                        let rai = (rafi >> 4) & 0xF;
                        let ra = self.decompile_reg(rai)?;
                        (Stmt::Pushq(ra), 2)
                    }
                    0xB0 => {
                        let rafi = bf.expect_u8()?;
                        let rai = (rafi >> 4) & 0xF;
                        let ra = self.decompile_reg(rai)?;
                        (Stmt::Popq(ra), 2)
                    }
                    0xFF => {
                        let num_sl = bf.slice(7)?;
                        let mut num_vec = Vec::from(num_sl);
                        num_vec.push(0);
                        let num = u64::from_le_bytes(num_vec[..].try_into()?);
                        (Stmt::DotQuad(num), 8)
                    }
                    other => {
                        bail!("invalid instruction indicator {}", other);
                    }
                };
                cmd_vec.push((addr, stmt));
                addr += len;
            }
        }
        Ok(cmd_vec)
    }

    fn gen_label(&mut self) -> String {
        let label = format!("Label_{}:", self.label_cnt);
        self.label_cnt += 1;
        label
    }

    fn decompile_reg(&self, ri: u8) -> anyhow::Result<Reg> {
        use Reg::*;
        Ok(match ri {
            0x0 => Rax,
            0x1 => Rcx,
            0x2 => Rdx,
            0x3 => Rbx,
            0x4 => Rsp,
            0x5 => Rbp,
            0x6 => Rsi,
            0x7 => Rdi,
            0x8 => R8,
            0x9 => R9,
            0xA => R10,
            0xB => R11,
            0xC => R12,
            0xD => R13,
            0xE => R14,
            _ => bail!("invalid reg indicator {}", ri),
        })
    }

    fn decompile_rr(&self, rri: u8) -> anyhow::Result<(Reg, Reg)> {
        let rai = (rri >> 4) & 0xF;
        let rbi = rri & 0xF;
        let ra = self.decompile_reg(rai)?;
        let rb = self.decompile_reg(rbi)?;
        Ok((ra, rb))
    }
}

#[derive(Parser)]
#[command(version, about, long_about=None)]
struct Cli {
    #[arg(value_name = "IN FILE")]
    in_file: PathBuf,

    #[arg(short = 'o', long, default_value_t = String::from("-"))]
    out_file: String,

    #[arg(short = 'd', long, default_value_t = false)]
    disassemble: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    if cli.disassemble {
        let disassemabled = disassemble(&cli.in_file)?;
        for (ln, stmt) in disassemabled {
            println!("0x{:x}: {}", ln, stmt);
        }
    } else {
        let bin_object = assemble(&cli.in_file)?;
        if cli.out_file == "-" {
            println!("{}", pretty_hex(&bin_object));
        } else {
            fs::write(&cli.out_file, &bin_object)?;
        }
    }
    Ok(())
}

fn assemble(in_file: &PathBuf) -> anyhow::Result<Vec<u8>> {
    let file_content = fs::read_to_string(in_file)?;
    let mut asm = Asm::new(&file_content);
    let stmts = asm.parse()?;
    let code_gen = Codegen::new(&stmts);
    let object = code_gen.compile()?;
    let executable = object.serialize();
    Ok(executable)
}

fn disassemble(in_file: &PathBuf) -> anyhow::Result<Vec<(u64, Stmt)>> {
    let bin_file = fs::read(in_file)?;
    let object = Object::deserialize(&bin_file)?;
    let mut disassembler = Disassembler::new(&object);
    disassembler.disassemble()
}

struct ByteBuf<'a> {
    buf: &'a [u8],
    pos: usize,
    len: usize,
}

impl<'a> ByteBuf<'a> {
    fn new(buf: &'a [u8]) -> Self {
        ByteBuf {
            buf,
            pos: 0,
            len: buf.len(),
        }
    }

    fn next_u8(&mut self) -> Option<u8> {
        if self.pos < self.len {
            self.pos += 1;
            Some(self.buf[self.pos - 1])
        } else {
            None
        }
    }

    fn expect_u8(&mut self) -> anyhow::Result<u8> {
        if self.pos < self.len {
            let val = self.buf[self.pos];
            self.pos += 1;
            Ok(val)
        } else {
            bail!("expect u8 but actual none");
        }
    }

    fn expect_u64(&mut self) -> anyhow::Result<u64> {
        if self.pos < self.len - 7 {
            let ar: [u8; 8] = self.buf[self.pos..self.pos + 8].try_into().unwrap();
            let val = u64::from_le_bytes(ar);
            self.pos += 8;
            Ok(val)
        } else {
            bail!("expect u64 but actual None");
        }
    }

    fn expect_i64(&mut self) -> anyhow::Result<i64> {
        if self.pos < self.len - 7 {
            let ar: [u8; 8] = self.buf[self.pos..self.pos + 8].try_into().unwrap();
            let val = i64::from_le_bytes(ar);
            self.pos += 8;
            Ok(val)
        } else {
            bail!("expect u64 but actual None");
        }
    }

    fn seek(&mut self, npos: usize) -> anyhow::Result<()> {
        if npos > self.len {
            bail!("seek out of range");
        }
        self.pos = npos;
        Ok(())
    }

    fn slice(&mut self, len: usize) -> anyhow::Result<&[u8]> {
        if self.pos + len <= self.len {
            let sl = &self.buf[self.pos..self.pos + len];
            self.pos += len;
            Ok(sl)
        } else {
            bail!("slice out of range");
        }
    }
}

#[derive(Debug)]
struct Fmeta {
    addr: u64,
    offset: u64,
    len: u64,
}

impl Fmeta {
    fn serialize(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&to_le_bytes(self.addr));
        bytes.extend_from_slice(&to_le_bytes(self.offset));
        bytes.extend_from_slice(&to_le_bytes(self.len));
        bytes
    }

    fn deserialize(slice: &[u8]) -> anyhow::Result<Fmeta> {
        let addr = u64::from_le_bytes(slice[0..8].try_into()?);
        let offset = u64::from_le_bytes(slice[8..16].try_into()?);
        let len = u64::from_le_bytes(slice[16..24].try_into()?);
        Ok(Fmeta { addr, offset, len })
    }
}

impl Object {
    fn serialize(&self) -> Vec<u8> {
        let mut bf = Vec::new();
        let mut bin_slices = Vec::new();
        let mut bin_offset = 16 + 24 * self.segments.len() as u64;
        bf.extend_from_slice(&to_le_bytes(self.entry));
        bf.extend_from_slice(&to_le_bytes(self.segments.len() as u64));
        for segment in &self.segments {
            let fmeta = Fmeta {
                addr: segment.addr,
                offset: bin_offset,
                len: segment.len(),
            };
            bf.extend_from_slice(&fmeta.serialize());
            bin_slices.push(&segment.binary);
            bin_offset += segment.len();
        }
        for bs in bin_slices {
            bf.extend_from_slice(bs);
        }
        bf
    }

    fn deserialize(bytes: &[u8]) -> anyhow::Result<Object> {
        let mut bf = ByteBuf::new(bytes);
        let entry = bf.expect_u64()?;
        let meta_cnt = bf.expect_u64()?;
        let mut metas = Vec::new();
        for _ in 0..meta_cnt {
            let meta = Fmeta::deserialize(bf.slice(24)?)?;
            metas.push(meta);
        }
        let mut segments = Vec::new();
        for meta in metas {
            let mut segment = Segment::new_at(meta.addr);
            bf.seek(meta.offset as usize)?;
            let bin_slice = bf.slice(meta.len as usize)?;
            segment.binary.extend_from_slice(bin_slice);
            segments.push(segment);
        }

        Ok(Object { entry, segments })
    }
}

fn to_le_bytes(n: u64) -> [u8; 8] {
    n.to_le_bytes()
}
