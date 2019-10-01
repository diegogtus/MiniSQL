package minisql;
import static minisql.Token.*;
%% 
%class Lexer
%type Token
%line
%column
%char

L=[a-b]
D=[0-9]
white=[ ,\r]

LineTerminator = \r|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]

/* comments */
// Comment can be the last line of the file, without line terminator.
EndOfLineComment     = "--" {InputCharacter}* {LineTerminator}?
CommentContent       = ( [^*] | \*+ [^/*] )*
DocumentationComment = "/**" {CommentContent} "*"+ "/" 
TraditionalComment   = "--" [^*] | "--"
MultiLine = "/*" {CommentContent} "*"+ "/"
MULTILINE_COMMENT = "/*" ~"*/" | "/*" "*"+ "/"
UNFINISHED_COMMENT = "/*" [^*\n]+
SINGLELINE_COMMENT = "--" [^\r\n]* [\r|\n|\r\n]?

Comment = {MultiLine} | {TraditionalComment} | {EndOfLineComment} | {DocumentationComment} | {MULTILINE_COMMENT} | {SINGLELINE_COMMENT}

IntConstant = [0-9]* 

BoolConstant = "0" | "1" | "NULL"

Identifier = [_a-zA-Z][_a-zA-Z0-9]*

/* Operadores */
ABSOLUTE = "ABSOLUTE"
ACTION = "ACTION"
ADA = "ADA"
ADD = "ADD"
ALL = "ALL"
ALLOCATE = "ALLOCATE"
ALTER = "ALTER"
AND = "AND"
ANY = "ANY"
ARE = "ARE"
AS = "AS"
ASC = "ASC"
ASSERTION = "ASSERTION"
AT = "AT"
AUTHORIZATION = "AUTHORIZATION"
AVG = "AVG"
BACKUP = "BACKUP"
BEGIN = "BEGIN"
BETWEEN = "BETWEEN"
BIT = "BIT"
BIT_LENGTH = "BIT_LENGTH"
BOTH = "BOTH"
BREAK = "BREAK"
BROWSE = "BROWSE"
BULK = "BULK"
BY = "BY"
CASCADE = "CASCADE"
CASCADED = "CASCADED"
CASE = "CASE"
CAST = "CAST"
CATALOG = "CATALOG"
CHAR = "CHAR"
CHAR_LENGTH = "CHAR_LENGTH"
CHARACTER = "CHARACTER"
CHARACTER_LENGTH = "CHARACTER_LENGTH"
CHECK = "CHECK"
CHECKPOINT = "CHECKPOINT"
CLOSE = "CLOSE"
CLUSTERED = "CLUSTERED"
COALESCE = "COALESCE"
COLLATE = "COLLATE"
COLLATION = "COLLATION"
COLUMN = "COLUMN"
COMMIT = "COMMIT"
COMPUTE = "COMPUTE"
CONNECT = "CONNECT"
CONNECTION = "CONNECTION"
CONSTRAINT = "CONSTRAINT"
CONSTRAINTS = "CONSTRAINTS"
CONTAINS = "CONTAINS"
CONTAINSTABLE = "CONTAINSTABLE"
CONTINUE = "CONTINUE"
CONVERT = "CONVERT"
CORRESPONDING = "CORRESPONDING"
COUNT = "COUNT"
CREATE = "CREATE"
CROSS = "CROSS"
CURRENT = "CURRENT"
CURRENT_DATE = "CURRENT_DATE"
CURRENT_TIME = "CURRENT_TIME"
CURRENT_TIMESTAMP = "CURRENT_TIMESTAMP"
CURRENT_USER = "CURRENT_USER"
CURSOR = "CURSOR"
DATABASE = "DATABASE"
DATE = "DATE"
DAY = "DAY"
DBCC = "DBCC"
DEALLOCATE = "DEALLOCATE"
DEC = "DEC"
DECIMAL = "DECIMAL"
DECLARE = "DECLARE"
DEFAULT = "DEFAULT"
DEFERRABLE = "DEFERRABLE"
DEFERRED = "DEFERRED"
DELETE = "DELETE"
DENY = "DENY"
DESC = "DESC"
DESCRIBE = "DESCRIBE"
DESCRIPTOR = "DESCRIPTOR"
DIAGNOSTICS = "DIAGNOSTICS"
DISCONNECT = "DISCONNECT"
DISK = "DISK"
DISTINCT = "DISTINCT"
DISTRIBUTED = "DISTRIBUTED"
DOMAIN = "DOMAIN"
DOUBLE = "DOUBLE"
DROP = "DROP"
DUMP = "DUMP"
ELSE = "ELSE"
END = "END"
END_EXEC = "END-EXEC"
ERRLVL = "ERRLVL"
ESCAPE = "ESCAPE"
EXCEPT = "EXCEPT"
EXCEPTION = "EXCEPTION"
EXEC = "EXEC"
EXECUTE = "EXECUTE"
EXISTS = "EXISTS"
EXIT = "EXIT"
EXTERNAL = "EXTERNAL"
EXTRACT = "EXTRACT"
FALSE = "FALSE"
FETCH = "FETCH"
FILE = "FILE"
FILLFACTOR = "FILLFACTOR"
FIRST = "FIRST"
FLOAT = "FLOAT"
FOR = "FOR"
FOREIGN = "FOREIGN"
FORTRAN = "FORTRAN"
FOUND = "FOUND"
FREETEXT = "FREETEXT"
FREETEXTTABLE = "FREETEXTTABLE"
FROM = "FROM"
FULL = "FULL"
FUNCTION = "FUNCTION"
GET = "GET"
GLOBAL = "GLOBAL"
GO = "GO"
GOTO = "GOTO"
GRANT = "GRANT"
GROUP = "GROUP"
HAVING = "HAVING"
HOLDLOCK = "HOLDLOCK"
HOUR = "HOUR"
IDENTITY = "IDENTITY"
IDENTITY_INSERT = "IDENTITY_INSERT"
IDENTITYCOL = "IDENTITYCOL"
IF = "IF"
IMMEDIATE = "IMMEDIATE"
IN = "IN"
INCLUDE = "INCLUDE"
INDEX = "INDEX"
INDICATOR = "INDICATOR"
INITIALLY = "INITIALLY"
INNER = "INNER"
INPUT = "INPUT"
INSENSITIVE = "INSENSITIVE"
INSERT = "INSERT"
INT = "INT"
INTEGER = "INTEGER"
INTERSECT = "INTERSECT"
INTERVAL = "INTERVAL"
INTO = "INTO"
IS = "IS"
ISOLATION = "ISOLATION"
JOIN = "JOIN"
KEY = "KEY"
KILL = "KILL"
LANGUAGE = "LANGUAGE"
LAST = "LAST"
LEADING = "LEADING"
LEFT = "LEFT"
LEVEL = "LEVEL"
LIKE = "LIKE"
LINENO = "LINENO"
LOAD = "LOAD"
LOCAL = "LOCAL"
LOWER = "LOWER"
MATCH = "MATCH"
MAX = "MAX"
MERGE = "MERGE"
MIN = "MIN"
MINUTE = "MINUTE"
MODULE = "MODULE"
MONTH = "MONTH"
NAMES = "NAMES"
NATIONAL = "NATIONAL"
NATURAL = "NATURAL"
NCHAR = "NCHAR"
NEXT = "NEXT"
NO = "NO"
NOCHECK = "NOCHECK"
NONCLUSTERED = "NONCLUSTERED"
NONE = "NONE"
NOT = "NOT"
NULL = "NULL"
NULLIF = "NULLIF"
NUMERIC = "NUMERIC"
OCTET_LENGTH = "OCTET_LENGTH"
OF = "OF"
OFF = "OFF"
OFFSETS = "OFFSETS"
ON = "ON"
ONLY = "ONLY"
OPEN = "OPEN"
OPENDATASOURCE = "OPENDATASOURCE"
OPENQUERY = "OPENQUERY"
OPENROWSET = "OPENROWSET"
OPENXML = "OPENXML"
OPTION = "OPTION"
OR = "OR"
ORDER = "ORDER"
OUTER = "OUTER"
OUTPUT = "OUTPUT"
OVER = "OVER"
OVERLAPS = "OVERLAPS"
PAD = "PAD"
PARTIAL = "PARTIAL"
PASCAL = "PASCAL"
PERCENT = "PERCENT"
PIVOT = "PIVOT"
PLAN = "PLAN"
POSITION = "POSITION"
PRECISION = "PRECISION"
PREPARE = "PREPARE"
PRESERVE = "PRESERVE"
PRIMARY = "PRIMARY"
PRINT = "PRINT"
PRIOR = "PRIOR"
PRIVILEGES = "PRIVILEGES"
PROC = "PROC"
PROCEDURE = "PROCEDURE"
PUBLIC = "PUBLIC"
RAISERROR = "RAISERROR"
READ = "READ"
READTEXT = "READTEXT"
REAL = "REAL"
RECONFIGURE = "RECONFIGURE"
REFERENCES = "REFERENCES"
RELATIVE = "RELATIVE"
REPLICATION = "REPLICATION"
RESTORE = "RESTORE"
RESTRICT = "RESTRICT"
RETURN = "RETURN"
REVERT = "REVERT"
REVOKE = "REVOKE"
RIGHT = "RIGHT"
ROLLBACK = "ROLLBACK"
ROWCOUNT = "ROWCOUNT"
ROWGUIDCOL = "ROWGUIDCOL"
ROWS = "ROWS"
RULE = "RULE"
SAVE = "SAVE"
SCHEMA = "SCHEMA"
SCROLL = "SCROLL"
SECOND = "SECOND"
SECTION = "SECTION"
SECURITYAUDIT = "SECURITYAUDIT"
SELECT = "SELECT"
SEMANTICKEYPHRASETABLE = "SEMANTICKEYPHRASETABLE"
SEMANTICSIMILARITYDETAILSTABLE = "SEMANTICSIMILARITYDETAILSTABLE"
SEMANTICSIMILARITYTABLE = "SEMANTICSIMILARITYTABLE"
SESSION = "SESSION"
SESSION_USER = "SESSION_USER"
SET = "SET"
SETUSER = "SETUSER"
SHUTDOWN = "SHUTDOWN"
SIZE = "SIZE"
SMALLINT = "SMALLINT"
SOME = "SOME"
SPACE = "SPACE"
SQL = "SQL"
SQLCA = "SQLCA"
SQLCODE = "SQLCODE"
SQLERROR = "SQLERROR"
SQLSTATE = "SQLSTATE"
SQLWARNING = "SQLWARNING"
STATISTICS = "STATISTICS"
SUBSTRING = "SUBSTRING"
SUM = "SUM"
SYSTEM_USER = "SYSTEM_USER"
TABLE = "TABLE"
TABLESAMPLE = "TABLESAMPLE"
TEMPORARY = "TEMPORARY"
TEXTSIZE = "TEXTSIZE"
THEN = "THEN"
TIME = "TIME"
TIMESTAMP = "TIMESTAMP"
TIMEZONE_HOUR = "TIMEZONE_HOUR"
TIMEZONE_MINUTE = "TIMEZONE_MINUTE"
TO = "TO"
TOP = "TOP"
TRAILING = "TRAILING"
TRAN = "TRAN"
TRANSACTION = "TRANSACTION"
TRANSLATE = "TRANSLATE"
TRANSLATION = "TRANSLATION"
TRIGGER = "TRIGGER"
TRIM = "TRIM"
TRUE = "TRUE"
TRUNCATE = "TRUNCATE"
TRY_CONVERT = "TRY_CONVERT"
TSEQUAL = "TSEQUAL"
UNION = "UNION"
UNIQUE = "UNIQUE"
UNKNOWN = "UNKNOWN"
UNPIVOT = "UNPIVOT"
UPDATE = "UPDATE"
UPDATETEXT = "UPDATETEXT"
UPPER = "UPPER"
USAGE = "USAGE"
USE = "USE"
USER = "USER"
USING = "USING"
VALUE = "VALUE"
VALUES = "VALUES"
VARCHAR = "VARCHAR"
VARYING = "VARYING"
VIEW = "VIEW"
WAITFOR = "WAITFOR"
WHEN = "WHEN"
WHENEVER = "WHENEVER"
WHERE = "WHERE"
WHILE = "WHILE"
WITH = "WITH"
WITHINGROUP = "WITHIN GROUP"
WORK = "WORK"
WRITE = "WRITE"
WRITETEXT = "WRITETEXT"
YEAR = "YEAR"
ZONE = "ZONE"

/*Operadores*/
 SUMA =  "+" 
 RESTA =  "-" 
 MULTIPLICACION =  "*" 
 DIVISION =  "/" 
 PORCENTAJE =  "%" 
 MENOR =  "<" 
 MENORIGUAL =  "<=" 
 MAYOR =  ">" 
 MAYORIGUAL =  ">=" 
 ASIGNAR =  "=" 
 IGUAL =  "==" 
 DIFERENTE =  "!=" 
 OP_AND =  "&&" 
 OP_OR =  "||"
 NEGACION =  "!" 
 PUNTOYCOMA =  ";" 
 COMA =  "," 
 PUNTO =  "." 
 CORCHETEIZQ =  "[" 
 CORCHETEDER =  "]" 
 PARENTESISIZQ =  "(" 
 PARENTESISDER =  ")" 
 LLAVEIZQ =  "{" 
 LLAVEDER =  "}" 
 CORCHETES =  "[]" 
 PARENTESIS =  "()" 
 LLAVES =  "{}" 
 ARROBA =  "@" 
 NUMERAL =  "#" 
 DOBLENUMERAL =  "##"

%{
	public String lexeme;
        public String column;
        public String line;
%}
%%
{WhiteSpace}                   { /* ignore */ }

{Comment}                      { /* ignore */ }
{BoolConstant} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return CONSTANTE_BOOLEANA;}
{IntConstant} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return CONSTANTE_ENTERA;}
[-+]?[0-9]+"."|[-+]?[0-9]+"."([0-9]+|("E"|"e")[-+]?[0-9]+|[0-9]+("E"|"e")[-+]?[0-9]+) {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FLOAT;}
['][^'\n]*['] { line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return STRING;}


{SUMA} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return SUMA;} 
{RESTA} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return RESTA;} 
{MULTIPLICACION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return MULTIPLICACION;} 
{DIVISION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return DIVISION;} 
{PORCENTAJE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return PORCENTAJE;} 
{MENOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return MENOR;} 
{MENORIGUAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return MENORIGUAL;} 
{MAYOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return MAYOR;} 
{MAYORIGUAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return MAYORIGUAL;} 
{ASIGNAR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return ASIGNAR;} 
{IGUAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return IGUAL;} 
{DIFERENTE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return DIFERENTE;} 
{AND} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return AND;} 
{OP_OR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return OP_OR;}  
{NEGACION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return NEGACION;} 
{PUNTOYCOMA} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return PUNTOYCOMA;} 
{COMA} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return COMA;} 
{PUNTO} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return PUNTO;} 
{CORCHETEIZQ} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return CORCHETEIZQ;} 
{CORCHETEDER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return CORCHETEDER;} 
{PARENTESISIZQ} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return PARENTESISIZQ;} 
{PARENTESISDER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return PARENTESISDER;} 
{LLAVEIZQ} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return LLAVEIZQ;} 
{LLAVEDER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return LLAVEDER;} 
{CORCHETES} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return CORCHETES;} 
{PARENTESIS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return PARENTESIS;} 
{LLAVES} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return LLAVES;} 
{ARROBA} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return ARROBA;} 
{NUMERAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return NUMERAL;} 
{DOBLENUMERAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return DOBLENUMERAL;}

{ABSOLUTE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ABSOLUTE;}
{ACTION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ACTION;}
{ADA} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ADA;}
{ADD} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ADD;}
{ALL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ALL;}
{ALLOCATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ALLOCATE;}
{ALTER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ALTER;}
{AND} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return AND;}
{ANY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ANY;}
{ARE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ARE;}
{AS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return AS;}
{ASC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ASC;}
{ASSERTION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ASSERTION;}
{AT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return AT;}
{AUTHORIZATION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return AUTHORIZATION;}
{AVG} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return AVG;}
{BACKUP} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BACKUP;}
{BEGIN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BEGIN;}
{BETWEEN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BETWEEN;}
{BIT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BIT;}
{BIT_LENGTH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BIT_LENGTH;}
{BOTH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BOTH;}
{BREAK} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BREAK;}
{BROWSE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BROWSE;}
{BULK} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BULK;}
{BY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return BY;}
{CASCADE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CASCADE;}
{CASCADED} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CASCADED;}
{CASE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CASE;}
{CAST} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CAST;}
{CATALOG} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CATALOG;}
{CHAR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CHAR;}
{CHAR_LENGTH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CHAR_LENGTH;}
{CHARACTER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CHARACTER;}
{CHARACTER_LENGTH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CHARACTER_LENGTH;}
{CHECK} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CHECK;}
{CHECKPOINT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CHECKPOINT;}
{CLOSE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CLOSE;}
{CLUSTERED} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CLUSTERED;}
{COALESCE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return COALESCE;}
{COLLATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return COLLATE;}
{COLLATION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return COLLATION;}
{COLUMN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return COLUMN;}
{COMMIT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return COMMIT;}
{COMPUTE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return COMPUTE;}
{CONNECT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CONNECT;}
{CONNECTION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CONNECTION;}
{CONSTRAINT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CONSTRAINT;}
{CONSTRAINTS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CONSTRAINTS;}
{CONTAINS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CONTAINS;}
{CONTAINSTABLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CONTAINSTABLE;}
{CONTINUE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CONTINUE;}
{CONVERT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CONVERT;}
{CORRESPONDING} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CORRESPONDING;}
{COUNT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return COUNT;}
{CREATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CREATE;}
{CROSS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CROSS;}
{CURRENT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CURRENT;}
{CURRENT_DATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CURRENT_DATE;}
{CURRENT_TIME} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CURRENT_TIME;}
{CURRENT_TIMESTAMP} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CURRENT_TIMESTAMP;}
{CURRENT_USER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CURRENT_USER;}
{CURSOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return CURSOR;}
{DATABASE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DATABASE;}
{DATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DATE;}
{DAY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DAY;}
{DBCC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DBCC;}
{DEALLOCATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DEALLOCATE;}
{DEC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DEC;}
{DECIMAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DECIMAL;}
{DECLARE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DECLARE;}
{DEFAULT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DEFAULT;}
{DEFERRABLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DEFERRABLE;}
{DEFERRED} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DEFERRED;}
{DELETE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DELETE;}
{DENY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DENY;}
{DESC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DESC;}
{DESCRIBE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DESCRIBE;}
{DESCRIPTOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DESCRIPTOR;}
{DIAGNOSTICS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DIAGNOSTICS;}
{DISCONNECT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DISCONNECT;}
{DISK} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DISK;}
{DISTINCT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DISTINCT;}
{DISTRIBUTED} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DISTRIBUTED;}
{DOMAIN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DOMAIN;}
{DOUBLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DOUBLE;}
{DROP} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DROP;}
{DUMP} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return DUMP;}
{ELSE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ELSE;}
{END} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return END;}
{END_EXEC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return END_EXEC;}
{ERRLVL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ERRLVL;}
{ESCAPE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ESCAPE;}
{EXCEPT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return EXCEPT;}
{EXCEPTION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return EXCEPTION;}
{EXEC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return EXEC;}
{EXECUTE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return EXECUTE;}
{EXISTS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return EXISTS;}
{EXIT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return EXIT;}
{EXTERNAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return EXTERNAL;}
{EXTRACT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return EXTRACT;}
{FALSE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FALSE;}
{FETCH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FETCH;}
{FILE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FILE;}
{FILLFACTOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FILLFACTOR;}
{FIRST} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FIRST;}
{FLOAT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FLOAT;}
{FOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FOR;}
{FOREIGN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FOREIGN;}
{FORTRAN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FORTRAN;}
{FOUND} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FOUND;}
{FREETEXT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FREETEXT;}
{FREETEXTTABLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FREETEXTTABLE;}
{FROM} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FROM;}
{FULL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FULL;}
{FUNCTION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FUNCTION;}
{GET} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return GET;}
{GLOBAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return GLOBAL;}
{GO} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return GO;}
{GOTO} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return GOTO;}
{GRANT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return GRANT;}
{GROUP} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return GROUP;}
{HAVING} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return HAVING;}
{HOLDLOCK} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return HOLDLOCK;}
{HOUR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return HOUR;}
{IDENTITY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return IDENTITY;}
{IDENTITY_INSERT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return IDENTITY_INSERT;}
{IDENTITYCOL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return IDENTITYCOL;}
{IF} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return IF;}
{IMMEDIATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return IMMEDIATE;}
{IN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return IN;}
{INCLUDE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INCLUDE;}
{INDEX} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INDEX;}
{INDICATOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INDICATOR;}
{INITIALLY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INITIALLY;}
{INNER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INNER;}
{INPUT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INPUT;}
{INSENSITIVE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INSENSITIVE;}
{INSERT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INSERT;}
{INT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INT;}
{INTEGER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INTEGER;}
{INTERSECT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INTERSECT;}
{INTERVAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INTERVAL;}
{INTO} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return INTO;}
{IS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return IS;}
{ISOLATION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ISOLATION;}
{JOIN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return JOIN;}
{KEY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return KEY;}
{KILL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return KILL;}
{LANGUAGE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LANGUAGE;}
{LAST} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LAST;}
{LEADING} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LEADING;}
{LEFT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LEFT;}
{LEVEL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LEVEL;}
{LIKE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LIKE;}
{LINENO} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LINENO;}
{LOAD} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LOAD;}
{LOCAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LOCAL;}
{LOWER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return LOWER;}
{MATCH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return MATCH;}
{MAX} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return MAX;}
{MERGE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return MERGE;}
{MIN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return MIN;}
{MINUTE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return MINUTE;}
{MODULE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return MODULE;}
{MONTH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return MONTH;}
{NAMES} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NAMES;}
{NATIONAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NATIONAL;}
{NATURAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NATURAL;}
{NCHAR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NCHAR;}
{NEXT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NEXT;}
{NO} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NO;}
{NOCHECK} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NOCHECK;}
{NONCLUSTERED} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NONCLUSTERED;}
{NONE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NONE;}
{NOT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NOT;}
{NULL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NULL;}
{NULLIF} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NULLIF;}
{NUMERIC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return NUMERIC;}
{OCTET_LENGTH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OCTET_LENGTH;}
{OF} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OF;}
{OFF} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OFF;}
{OFFSETS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OFFSETS;}
{ON} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ON;}
{ONLY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ONLY;}
{OPEN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OPEN;}
{OPENDATASOURCE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OPENDATASOURCE;}
{OPENQUERY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OPENQUERY;}
{OPENROWSET} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OPENROWSET;}
{OPENXML} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OPENXML;}
{OPTION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OPTION;}
{OR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OR;}
{ORDER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ORDER;}
{OUTER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OUTER;}
{OUTPUT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OUTPUT;}
{OVER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OVER;}
{OVERLAPS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return OVERLAPS;}
{PAD} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PAD;}
{PARTIAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PARTIAL;}
{PASCAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PASCAL;}
{PERCENT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PERCENT;}
{PIVOT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PIVOT;}
{PLAN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PLAN;}
{POSITION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return POSITION;}
{PRECISION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PRECISION;}
{PREPARE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PREPARE;}
{PRESERVE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PRESERVE;}
{PRIMARY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PRIMARY;}
{PRINT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PRINT;}
{PRIOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PRIOR;}
{PRIVILEGES} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PRIVILEGES;}
{PROC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PROC;}
{PROCEDURE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PROCEDURE;}
{PUBLIC} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return PUBLIC;}
{RAISERROR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RAISERROR;}
{READ} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return READ;}
{READTEXT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return READTEXT;}
{REAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return REAL;}
{RECONFIGURE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RECONFIGURE;}
{REFERENCES} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return REFERENCES;}
{RELATIVE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RELATIVE;}
{REPLICATION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return REPLICATION;}
{RESTORE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RESTORE;}
{RESTRICT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RESTRICT;}
{RETURN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RETURN;}
{REVERT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return REVERT;}
{REVOKE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return REVOKE;}
{RIGHT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RIGHT;}
{ROLLBACK} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ROLLBACK;}
{ROWCOUNT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ROWCOUNT;}
{ROWGUIDCOL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ROWGUIDCOL;}
{ROWS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ROWS;}
{RULE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RULE;}
{SAVE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SAVE;}
{SCHEMA} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SCHEMA;}
{SCROLL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SCROLL;}
{SECOND} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SECOND;}
{SECTION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SECTION;}
{SECURITYAUDIT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SECURITYAUDIT;}
{SELECT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SELECT;}
{SEMANTICKEYPHRASETABLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SEMANTICKEYPHRASETABLE;}
{SEMANTICSIMILARITYDETAILSTABLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SEMANTICSIMILARITYDETAILSTABLE;}
{SEMANTICSIMILARITYTABLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SEMANTICSIMILARITYTABLE;}
{SESSION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SESSION;}
{SESSION_USER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SESSION_USER;}
{SET} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SET;}
{SETUSER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SETUSER;}
{SHUTDOWN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SHUTDOWN;}
{SIZE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SIZE;}
{SMALLINT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SMALLINT;}
{SOME} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SOME;}
{SPACE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SPACE;}
{SQL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SQL;}
{SQLCA} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SQLCA;}
{SQLCODE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SQLCODE;}
{SQLERROR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SQLERROR;}
{SQLSTATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SQLSTATE;}
{SQLWARNING} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SQLWARNING;}
{STATISTICS} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return STATISTICS;}
{SUBSTRING} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SUBSTRING;}
{SUM} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SUM;}
{SYSTEM_USER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return SYSTEM_USER;}
{TABLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TABLE;}
{TABLESAMPLE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TABLESAMPLE;}
{TEMPORARY} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TEMPORARY;}
{TEXTSIZE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TEXTSIZE;}
{THEN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return THEN;}
{TIME} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TIME;}
{TIMESTAMP} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TIMESTAMP;}
{TIMEZONE_HOUR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TIMEZONE_HOUR;}
{TIMEZONE_MINUTE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TIMEZONE_MINUTE;}
{TO} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TO;}
{TOP} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TOP;}
{TRAILING} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRAILING;}
{TRAN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRAN;}
{TRANSACTION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRANSACTION;}
{TRANSLATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRANSLATE;}
{TRANSLATION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRANSLATION;}
{TRIGGER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRIGGER;}
{TRIM} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRIM;}
{TRUE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRUE;}
{TRUNCATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRUNCATE;}
{TRY_CONVERT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TRY_CONVERT;}
{TSEQUAL} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return TSEQUAL;}
{UNION} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return UNION;}
{UNIQUE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return UNIQUE;}
{UNKNOWN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return UNKNOWN;}
{UNPIVOT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return UNPIVOT;}
{UPDATE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return UPDATE;}
{UPDATETEXT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return UPDATETEXT;}
{UPPER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return UPPER;}
{USAGE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return USAGE;}
{USE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return USE;}
{USER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return USER;}
{USING} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return USING;}
{VALUE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return VALUE;}
{VALUES} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return VALUES;}
{VARCHAR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return VARCHAR;}
{VARYING} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return VARYING;}
{VIEW} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return VIEW;}
{WAITFOR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WAITFOR;}
{WHEN} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WHEN;}
{WHENEVER} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WHENEVER;}
{WHERE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WHERE;}
{WHILE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WHILE;}
{WITH} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WITH;}
{WITHINGROUP} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WITHINGROUP;}
{WORK} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WORK;}
{WRITE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WRITE;}
{WRITETEXT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return WRITETEXT;}
{YEAR} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return YEAR;}
{ZONE} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return ZONE;}

{Identifier} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return IDENTIFICADOR;}

[^]   {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return ERROR;}
{UNFINISHED_COMMENT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return ERROR;} 