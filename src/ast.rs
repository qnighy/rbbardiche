use crate::token::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Range(pub usize, pub usize);

impl std::ops::BitOr for Range {
    type Output = Range;

    fn bitor(self, rhs: Self) -> Self::Output {
        Range(std::cmp::min(self.0, rhs.0), std::cmp::max(self.1, rhs.1))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeMeta {
    pub range: Range,
    pub node_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DelimitedElement<T, D> {
    pub inner: T,
    pub delimiter: Option<D>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
    pub meta: NodeMeta,
}

impl AsRef<NodeMeta> for Program {
    fn as_ref(&self) -> &NodeMeta {
        &self.meta
    }
}

impl AsMut<NodeMeta> for Program {
    fn as_mut(&mut self) -> &mut NodeMeta {
        &mut self.meta
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Parenthesized(ParenthesizedExpr),
    Compound(CompoundExpr),
    // TODO: bigint, float, etc.
    Numeric(NumericExpr),
    Symbol(SymbolExpr),
    StringLiteral(StringLiteralExpr),
    TernaryCond(TernaryCondExpr),
    Range(RangeExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Nil(NilExpr),
    Assign(AssignExpr),
    Send(SendExpr),
    Const(ConstExpr),
    Array(ArrayExpr),
    Class(ClassExpr),
    Module(ModuleExpr),
    Defn(DefnExpr),
    Errored(ErroredExpr),
}

#[macro_export]
macro_rules! delegate_expr {
    ($e:expr, $x:ident => $arm:expr) => {
        match $e {
            $crate::ast::Expr::Parenthesized($x) => $arm,
            $crate::ast::Expr::Compound($x) => $arm,
            $crate::ast::Expr::Numeric($x) => $arm,
            $crate::ast::Expr::Symbol($x) => $arm,
            $crate::ast::Expr::StringLiteral($x) => $arm,
            $crate::ast::Expr::TernaryCond($x) => $arm,
            $crate::ast::Expr::Range($x) => $arm,
            $crate::ast::Expr::Binary($x) => $arm,
            $crate::ast::Expr::Unary($x) => $arm,
            $crate::ast::Expr::Nil($x) => $arm,
            $crate::ast::Expr::Assign($x) => $arm,
            $crate::ast::Expr::Send($x) => $arm,
            $crate::ast::Expr::Const($x) => $arm,
            $crate::ast::Expr::Array($x) => $arm,
            $crate::ast::Expr::Class($x) => $arm,
            $crate::ast::Expr::Module($x) => $arm,
            $crate::ast::Expr::Defn($x) => $arm,
            $crate::ast::Expr::Errored($x) => $arm,
        }
    };
}

impl Expr {
    pub fn meta(&self) -> &NodeMeta {
        self.as_ref()
    }

    pub fn meta_mut(&mut self) -> &mut NodeMeta {
        self.as_mut()
    }

    pub fn range(&self) -> Range {
        self.meta().range
    }
}

impl AsRef<NodeMeta> for Expr {
    fn as_ref(&self) -> &NodeMeta {
        delegate_expr!(self, x => &x.meta)
    }
}

impl AsMut<NodeMeta> for Expr {
    fn as_mut(&mut self) -> &mut NodeMeta {
        delegate_expr!(self, x => &mut x.meta)
    }
}

impl From<ParenthesizedExpr> for Expr {
    fn from(e: ParenthesizedExpr) -> Self {
        Expr::Parenthesized(e)
    }
}

impl From<CompoundExpr> for Expr {
    fn from(e: CompoundExpr) -> Self {
        Expr::Compound(e)
    }
}

impl From<NumericExpr> for Expr {
    fn from(e: NumericExpr) -> Self {
        Expr::Numeric(e)
    }
}

impl From<SymbolExpr> for Expr {
    fn from(e: SymbolExpr) -> Self {
        Expr::Symbol(e)
    }
}

impl From<StringLiteralExpr> for Expr {
    fn from(e: StringLiteralExpr) -> Self {
        Expr::StringLiteral(e)
    }
}

impl From<TernaryCondExpr> for Expr {
    fn from(e: TernaryCondExpr) -> Self {
        Expr::TernaryCond(e)
    }
}

impl From<RangeExpr> for Expr {
    fn from(e: RangeExpr) -> Self {
        Expr::Range(e)
    }
}

impl From<BinaryExpr> for Expr {
    fn from(e: BinaryExpr) -> Self {
        Expr::Binary(e)
    }
}

impl From<UnaryExpr> for Expr {
    fn from(e: UnaryExpr) -> Self {
        Expr::Unary(e)
    }
}

impl From<NilExpr> for Expr {
    fn from(e: NilExpr) -> Self {
        Expr::Nil(e)
    }
}

impl From<AssignExpr> for Expr {
    fn from(e: AssignExpr) -> Self {
        Expr::Assign(e)
    }
}

impl From<SendExpr> for Expr {
    fn from(e: SendExpr) -> Self {
        Expr::Send(e)
    }
}

impl From<ConstExpr> for Expr {
    fn from(e: ConstExpr) -> Self {
        Expr::Const(e)
    }
}

impl From<ArrayExpr> for Expr {
    fn from(e: ArrayExpr) -> Self {
        Expr::Array(e)
    }
}

impl From<ClassExpr> for Expr {
    fn from(e: ClassExpr) -> Self {
        Expr::Class(e)
    }
}

impl From<ModuleExpr> for Expr {
    fn from(e: ModuleExpr) -> Self {
        Expr::Module(e)
    }
}

impl From<DefnExpr> for Expr {
    fn from(e: DefnExpr) -> Self {
        Expr::Defn(e)
    }
}

impl From<ErroredExpr> for Expr {
    fn from(e: ErroredExpr) -> Self {
        Expr::Errored(e)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParenthesizedExpr {
    pub stmts: Vec<Stmt>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundExpr {
    pub stmts: Vec<Stmt>,
    pub meta: NodeMeta,
}

// TODO: bigint, float, etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumericExpr {
    pub numval: i32,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolExpr {
    pub open_token: Token,
    pub ident_token: Token,
    pub value: String,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteralExpr {
    pub strval: String,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TernaryCondExpr {
    pub cond: Box<Expr>,
    pub consequence: Box<Expr>,
    pub alternate: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RangeExpr {
    pub begin: Option<Box<Expr>>,
    pub range_type: RangeType,
    pub end: Option<Box<Expr>>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NilExpr {
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SendExpr {
    pub optional: bool,
    pub recv: Option<Box<Expr>>,
    pub name: String,
    pub args: Option<Args>,
    pub meta: NodeMeta,
}

impl SendExpr {
    pub fn set_args(&mut self, args: Args) {
        assert!(self.args.is_none());
        self.meta.range = self.meta.range | args.range();
        self.args = Some(args);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Args {
    Paren(ParenArgs),
    Command(CommandArgs),
}

impl Args {
    pub fn list(&self) -> &Vec<DelimitedArg> {
        match self {
            Args::Paren(e) => &e.list,
            Args::Command(e) => &e.list,
        }
    }

    pub fn list_mut(&mut self) -> &mut Vec<DelimitedArg> {
        match self {
            Args::Paren(e) => &mut e.list,
            Args::Command(e) => &mut e.list,
        }
    }

    pub fn meta(&self) -> &NodeMeta {
        self.as_ref()
    }

    pub fn meta_mut(&mut self) -> &mut NodeMeta {
        self.as_mut()
    }

    pub fn range(&self) -> Range {
        self.meta().range
    }
}

impl AsRef<NodeMeta> for Args {
    fn as_ref(&self) -> &NodeMeta {
        match self {
            Args::Paren(e) => &e.meta,
            Args::Command(e) => &e.meta,
        }
    }
}

impl AsMut<NodeMeta> for Args {
    fn as_mut(&mut self) -> &mut NodeMeta {
        match self {
            Args::Paren(e) => &mut e.meta,
            Args::Command(e) => &mut e.meta,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParenArgs {
    pub open_token: Token,
    pub list: Vec<DelimitedArg>,
    pub close_token: Option<Token>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommandArgs {
    pub list: Vec<DelimitedArg>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DelimitedArg {
    pub arg: Arg,
    pub debris: Vec<Debri>,
    pub delim: Option<Token>,
    pub meta: NodeMeta,
}

impl DelimitedArg {
    pub fn range(&self) -> Range {
        self.meta.range
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Simple(Expr),
    // Splat(SplatArg),
    // KeywordSplat(KeywordSplatArg),
    // Assoc(AssocArg),
    // Labeled(LabeledArg),
    // Block(BlockArg),
}

impl Arg {
    pub fn meta(&self) -> &NodeMeta {
        match self {
            Arg::Simple(a) => a.meta(),
        }
    }

    pub fn meta_mut(&mut self) -> &mut NodeMeta {
        match self {
            Arg::Simple(a) => a.meta_mut(),
        }
    }

    pub fn range(&self) -> Range {
        self.meta().range
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstExpr {
    /// If true, this expression starts with `::`.
    /// Cannot coexist with recv.
    pub toplevel: bool,
    /// Expression before the token `::`.
    pub recv: Option<Box<Expr>>,
    pub name: String,
    pub meta: NodeMeta,
}

impl ConstExpr {
    pub fn convertible_to_send(&self) -> bool {
        !self.toplevel
    }

    pub fn convert_to_send(self) -> SendExpr {
        assert!(self.convertible_to_send());
        SendExpr {
            optional: false,
            recv: self.recv,
            name: self.name,
            args: None,
            meta: self.meta,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayExpr {
    pub open_token: Token,
    pub list: Vec<DelimitedArg>,
    pub close_token: Option<Token>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassExpr {
    pub cpath: Box<Expr>,
    pub superclass: Option<SuperclassClause>,
    pub body: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuperclassClause {
    pub expr: Box<Expr>,
    pub meta: NodeMeta,
}

impl SuperclassClause {
    pub fn range(&self) -> Range {
        self.meta.range
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleExpr {
    pub cpath: Box<Expr>,
    pub body: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefnExpr {
    pub name: String,
    pub args: Option<FArgs>,
    pub body: Box<Expr>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FArgs {
    Paren(ParenFArgs),
    Command(CommandFArgs),
}

impl FArgs {
    pub fn list(&self) -> &Vec<DelimitedFArg> {
        match self {
            FArgs::Paren(e) => &e.list,
            FArgs::Command(e) => &e.list,
        }
    }

    pub fn list_mut(&mut self) -> &mut Vec<DelimitedFArg> {
        match self {
            FArgs::Paren(e) => &mut e.list,
            FArgs::Command(e) => &mut e.list,
        }
    }

    pub fn meta(&self) -> &NodeMeta {
        self.as_ref()
    }

    pub fn meta_mut(&mut self) -> &mut NodeMeta {
        self.as_mut()
    }

    pub fn range(&self) -> Range {
        self.meta().range
    }
}

impl AsRef<NodeMeta> for FArgs {
    fn as_ref(&self) -> &NodeMeta {
        match self {
            FArgs::Paren(e) => &e.meta,
            FArgs::Command(e) => &e.meta,
        }
    }
}

impl AsMut<NodeMeta> for FArgs {
    fn as_mut(&mut self) -> &mut NodeMeta {
        match self {
            FArgs::Paren(e) => &mut e.meta,
            FArgs::Command(e) => &mut e.meta,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParenFArgs {
    pub open_token: Token,
    pub list: Vec<DelimitedFArg>,
    pub close_token: Option<Token>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommandFArgs {
    pub list: Vec<DelimitedFArg>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DelimitedFArg {
    pub arg: FArg,
    pub debris: Vec<Debri>,
    pub delim: Option<Token>,
    pub meta: NodeMeta,
}

impl DelimitedFArg {
    pub fn range(&self) -> Range {
        self.meta.range
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FArg {
    Simple(Expr),
    // Splat(SplatFArg),
    // KeywordSplat(KeywordSplatFArg),
    // Assoc(AssocFArg),
    // Labeled(LabeledFArg),
    // Block(BlockFArg),
}

impl FArg {
    pub fn meta(&self) -> &NodeMeta {
        match self {
            FArg::Simple(a) => a.meta(),
        }
    }

    pub fn meta_mut(&mut self) -> &mut NodeMeta {
        match self {
            FArg::Simple(a) => a.meta_mut(),
        }
    }

    pub fn range(&self) -> Range {
        self.meta().range
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErroredExpr {
    pub debris: Vec<Debri>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr(ExprStmt),
    Empty(EmptyStmt),
}

impl Stmt {
    pub fn meta(&self) -> &NodeMeta {
        match self {
            Stmt::Expr(s) => &s.meta,
            Stmt::Empty(s) => &s.meta,
        }
    }

    pub fn meta_mut(&mut self) -> &mut NodeMeta {
        match self {
            Stmt::Expr(s) => &mut s.meta,
            Stmt::Empty(s) => &mut s.meta,
        }
    }

    pub fn range(&self) -> Range {
        self.meta().range
    }
}

impl From<ExprStmt> for Stmt {
    fn from(s: ExprStmt) -> Self {
        Stmt::Expr(s)
    }
}

impl From<EmptyStmt> for Stmt {
    fn from(s: EmptyStmt) -> Self {
        Stmt::Empty(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub debris: Vec<Debri>,
    pub delim: Option<Token>,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyStmt {
    pub delim: Token,
    pub meta: NodeMeta,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeType {
    /// `..`
    Inclusive,
    /// `...`
    Exclusive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// `||`
    LogicalOr,
    /// `&&`
    LogicalAnd,
    /// `<=>`
    Cmp,
    /// `==`
    Eq,
    /// `===`
    Eqq,
    /// `!=`
    NEq,
    /// `=~`
    Match,
    /// `!~`
    NMatch,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `|`
    BitwiseOr,
    /// `^`
    BitwiseXor,
    /// `&`
    BitwiseAnd,
    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `**`
    Pow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `+`
    Plus,
    /// `-`
    Neg,
    /// `!`
    Not,
    /// `~`
    BitwiseNot,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Debri {
    Token(Token),
    ExprLike(Expr),
}

impl Debri {
    pub fn range(&self) -> Range {
        match self {
            Debri::Token(token) => token.range,
            Debri::ExprLike(expr) => expr.range(),
        }
    }
}
