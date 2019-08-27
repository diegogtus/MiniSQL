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
MULTILINE_COMMENT = "/*" [^*] ~"*/" | "/*" "*"+ "/"
UNFINISHED_COMMENT = "/*" [^*]+
SINGLELINE_COMMENT = "--" [^\r\n]* [\r|\n|\r\n]?

Comment = {MultiLine} | {TraditionalComment} | {EndOfLineComment} | {DocumentationComment} | {MULTILINE_COMMENT} | {SINGLELINE_COMMENT}

IntConstant = [0-9]* | "0x"[0-9a-fA-F]+ |"0X"[0-9a-fA-F]+ 

BoolConstant = "0" | "1" | "NULL"

Identifier = [_a-zA-Z][_a-zA-Z0-9]*

Reserved = "ABSOLUTE"|"ACTION"|"ADA"|"ADD"|"ALL"|"ALLOCATE"|"ALTER"|"AND"|"ANY"|"ARE"|"AS"|"ASC"|
        "ASSERTION"|"AT"|"AUTHORIZATION"|"AVG"|"BACKUP"|"BEGIN"|"BETWEEN"|"BIT"|
        "BIT_LENGTH"|"BOTH"|"BREAK"|"BROWSE"|"BULK"|"BY"|"CASCADE"|"CASCADED"|
        "CASE"|"CAST"|"CATALOG"|"CHAR"|"CHAR_LENGTH"|"CHARACTER"|"CHARACTER_LENGTH"|
        "CHECK"|"CHECKPOINT"|"CLOSE"|"CLUSTERED"|"COALESCE"|"COLLATE"|"COLLATION"|
        "COLUMN"|"COMMIT"|"COMPUTE"|"CONNECT"|"CONNECTION"|"CONSTRAINT"|"CONSTRAINTS"|
        "CONTAINS"|"CONTAINSTABLE"|"CONTINUE"|"CONVERT"|"CORRESPONDING"|"COUNT"|
        "CREATE"|"CROSS"|"CURRENT"|"CURRENT_DATE"|"CURRENT_TIME"|"CURRENT_TIMESTAMP"|
        "CURRENT_USER"|"CURSOR"|"DATABASE"|"DATE"|"DAY"|"DBCC"|"DEALLOCATE"|"DEC"|
        "DECIMAL"|"DECLARE"|"DEFAULT"|"DEFERRABLE"|"DEFERRED"|"DELETE"|"DENY"|"DESC"|
        "DESCRIBE"|"DESCRIPTOR"|"DIAGNOSTICS"|"DISCONNECT"|"DISK"|"DISTINCT"|"DISTRIBUTED"|
        "DOMAIN"|"DOUBLE"|"DROP"|"DUMP"|"ELSE"|"END"|"END-EXEC"|"ERRLVL"|"ESCAPE"|"EXCEPT"|
        "EXCEPTION"|"EXEC"|"EXECUTE"|"EXISTS"|"EXIT"|"EXTERNAL"|"EXTRACT"|"FALSE"|
        "FETCH"|"FILE"|"FILLFACTOR"|"FIRST"|"FLOAT"|"FOR"|"FOREIGN"|"FORTRAN"|"FOUND"|
        "FREETEXT"|"FREETEXTTABLE"|"FROM"|"FULL"|"FUNCTION"|"GET"|"GLOBAL"|"GO"|"GOTO"|
        "GRANT"|"GROUP"|"HAVING"|"HOLDLOCK"|"HOUR"|"IDENTITY"|"IDENTITY_INSERT"|
        "IDENTITYCOL"|"IF"|"IMMEDIATE"|"IN"|"INCLUDE"|"INDEX"|"INDICATOR"|"INITIALLY"|
        "INNER"|"INPUT"|"INSENSITIVE"|"INSERT"|"INT"|"INTEGER"|"INTERSECT"|"INTERVAL"|
        "INTO"|"IS"|"ISOLATION"|"JOIN"|"KEY"|"KILL"|"LANGUAGE"|"LAST"|"LEADING"|"LEFT"|
        "LEVEL"|"LIKE"|"LINENO"|"LOAD"|"LOCAL"|"LOWER"|"MATCH"|"MAX"|"MERGE"|"MIN"|
        "MINUTE"|"MODULE"|"MONTH"|"NAMES"|"NATIONAL"|"NATURAL"|"NCHAR"|"NEXT"|"NO"|
        "NOCHECK"|"NONCLUSTERED"|"NONE"|"NOT"|"NULL"|"NULLIF"|"NUMERIC"|"OCTET_LENGTH"|
        "OF"|"OFF"|"OFFSETS"|"ON"|"ONLY"|"OPEN"|"OPENDATASOURCE"|"OPENQUERY"|"OPENROWSET"|
        "OPENXML"|"OPTION"|"OR"|"ORDER"|"OUTER"|"OUTPUT"|"OVER"|"OVERLAPS"|"PAD"|"PARTIAL"|
        "PASCAL"|"PERCENT"|"PIVOT"|"PLAN"|"POSITION"|"PRECISION"|"PREPARE"|"PRESERVE"|
        "PRIMARY"|"PRINT"|"PRIOR"|"PRIVILEGES"|"PROC"|"PROCEDURE"|"PUBLIC"|"RAISERROR"|
        "READ"|"READTEXT"|"REAL"|"RECONFIGURE"|"REFERENCES"|"RELATIVE"|"REPLICATION"|
        "RESTORE"|"RESTRICT"|"RETURN"|"REVERT"|"REVOKE"|"RIGHT"|"ROLLBACK"|"ROWCOUNT"|
        "ROWGUIDCOL"|"ROWS"|"RULE"|"SAVE"|"SCHEMA"|"SCROLL"|"SECOND"|"SECTION"|
        "SECURITYAUDIT"|"SELECT"|"SEMANTICKEYPHRASETABLE"|"SEMANTICSIMILARITYDETAILSTABLE"|
        "SEMANTICSIMILARITYTABLE"|"SESSION"|"SESSION_USER"|"SET"|"SETUSER"|"SHUTDOWN"|"SIZE"|
        "SMALLINT"|"SOME"|"SPACE"|"SQL"|"SQLCA"|"SQLCODE"|"SQLERROR"|"SQLSTATE"|"SQLWARNING"|
        "STATISTICS"|"SUBSTRING"|"SUM"|"SYSTEM_USER"|"TABLE"|"TABLESAMPLE"|"TEMPORARY"|
        "TEXTSIZE"|"THEN"|"TIME"|"TIMESTAMP"|"TIMEZONE_HOUR"|"TIMEZONE_MINUTE"|"TO"|
        "TOP"|"TRAILING"|"TRAN"|"TRANSACTION"|"TRANSLATE"|"TRANSLATION"|"TRIGGER"|
        "TRIM"|"TRUE"|"TRUNCATE"|"TRY_CONVERT"|"TSEQUAL"|"UNION"|"UNIQUE"|"UNKNOWN"|
        "UNPIVOT"|"UPDATE"|"UPDATETEXT"|"UPPER"|"USAGE"|"USE"|"USER"|"USING"|"VALUE"|
        "VALUES"|"VARCHAR"|"VARYING"|"VIEW"|"WAITFOR"|"WHEN"|"WHENEVER"|"WHERE"|
        "WHILE"|"WITH"|"WITHIN GROUP"|"WORK"|"WRITE"|"WRITETEXT"|"YEAR"|"ZONE"


Puntuaction="+"| "-"| "*"| "/"| "%"| "<"| "<="| ">"| ">="| "="| "=="| "!="| 
        "&&"| "||"| "!"| ";"| ","| "."| "["| "]"| "("| ")"| "{"| "}"| "[]"| 
        "()"| "{}" | "@" | "#" | "##" 

%{
	public String lexeme;
        public String column;
        public String line;
%}
%%


[\n] {return SALTO;}

{Reserved} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return RESERVADA;}
{Identifier} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return IDENTIFICADOR;}
{WhiteSpace}                   { /* ignore */ }
{Puntuaction} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return PUNTUACION;}
{Comment}                      { /* ignore */ }
{BoolConstant} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return CONSTANTE_BOOLEANA;}
{IntConstant} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return CONSTANTE_ENTERA;}
[-+]?[0-9]+"."|[-+]?[0-9]+"."([0-9]+|("E"|"e")[-+]?[0-9]+|[0-9]+("E"|"e")[-+]?[0-9]+) {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1); return FLOAT;}
['][^'\n]*['] { line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return STRING;}
{Puntuaction}                   { line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return OPERADOR;}

[^]   {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return ERROR;}
{UNFINISHED_COMMENT} {line=Integer.toString(yyline+1);column=Integer.toString(yycolumn+1);return ERROR;} 