%{
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

int at_count;
int arg;
char print_buffer[1024];
int begin_count = 0;
int in_vardecl = 0;
int printed = -1;
int underscore_count = 0;
int invalid_char = 0;
int var_count = 0;
char var_arr[100][50];

int validate_integer(const char* num_str, int base) {
for (int i = 0; num_str[i] != '\0'; i++) {
if (base == 2 && (num_str[i] != '0' && num_str[i] != '1')) {
return 0;
} else if (base == 8 && (num_str[i] < '0' || num_str[i] > '7')) {
return 0;
} else if (base == 10 && !isdigit(num_str[i])) {
return 0;
}
}
return 1;
}

int is_declared(char *var) {
for (int i = 0; i < var_count; i++) {
if (strcmp(var_arr[i], var) == 0) {
return 1;
}
}
return 0;
}

void push_var(const char *var) {
if (var_count < 100) {
strcpy(var_arr[var_count], var);
var_count++;
}
}

int count_underscores(const char *str) {
int count = 0;
for (int i = 0; str[i] != '\0'; i++) {
if (str[i] == '_') count++;
}
return count;
}

int is_keyword(const char *str) {
const char *keywords[] = {"int", "char", "if", "else", "while", "for", "main",
"begin", "end", "print", "scan", "program", "inc", "dec", "then"};
int num_keywords = sizeof(keywords) / sizeof(keywords[0]);

for (int i = 0; i < num_keywords; i++) {
if (strcmp(str, keywords[i]) == 0) {
return 1;
}
}
return 0;
}
%}

%option noyywrap

lowalpha [a-z]
upalpha [A-Z]
digit [0-9]
identifier [a-zA-Z][a-zA-Z0-9_]*
keyword (int|char|if|else|while|for|main|begin|end|print|scan|program|inc|dec|then)
assignment_operator [:+\-*\/%][=]

%%

"begin program:" {
begin_count++;
printf("begin\tKeyword\n");
printf("program\tKeyword\n");
printf(":\tSeparator\n");
}

"end program" {
begin_count--;
printf("end\tKeyword\n");
printf("program\tKeyword\n");
}

"begin VarDecl:" {
in_vardecl = 1;
begin_count++;
printf("begin\tKeyword\n");
printf("VarDecl\tKeyword\n");
printf(":\tSeparator\n");
}

"end VarDecl" {
in_vardecl = 0;
begin_count--;
printf("end\tKeyword\n");
printf("VarDecl\tKeyword\n");
}

{keyword} { printf("%s\tKeyword\n", yytext); }

[+\-*/%] { printf("%s\tArithmetic Operator\n", yytext); }
">"|"<"|">="|"<="|"<>" { printf("%s\tRelational Operator\n", yytext); }
{assignment_operator} { printf("%s\tAssignment Operator\n", yytext); }

"print(" {
at_count = 0;
arg = 0;
strcpy(print_buffer, "print(");
printed = 1;
}

"while" { 
    printf("while\tKeyword\n");
}

"do" {
    printf("do\tKeyword\n");
}


"scan(" {
at_count = 0;
arg = 0;
strcpy(print_buffer, "scan(");
printed = 0;
}

\"([^\"\\]|\\.)*\" {
if (printed == 0) {
strcat(print_buffer, yytext);
for (int i = 0; yytext[i]; i++) {
if (yytext[i] == '@') at_count++;
}
}
printf("%s\tString Literal\n", yytext);
}

{identifier} 
{if (printed != -1) {
        strcat(print_buffer, yytext);
        arg++;
    }
    
    if (is_keyword(yytext)) {
        printf("%s\tKeyword\n", yytext);
    } else if (!is_declared(yytext) && !in_vardecl) {
        printf("%s\tError: undefined identifier\n", yytext);
    } else {
        printf("%s\tIdentifier\n", yytext);
    }
if (printed == 0) {
strcat(print_buffer, yytext);
arg++;
}

}

"," {
if (printed == 0) {
strcat(print_buffer, ",");
}
printf(",\tSeparator\n");
}

");" {
strcat(print_buffer, ");");
if (printed == 0) {
char *content = strdup(print_buffer + 5); // Skip "scan("
content[strlen(content) - 2] = '\0'; // Remove ");"

int comma_count = 0;
for (int i = 0; content[i]; i++) {
if (content[i] == ',') comma_count++;
}

if (at_count == arg && at_count > 0 && comma_count == at_count) {
printf("%s\tValid input statement\n", print_buffer);
} else {
printf("%s\tError: Invalid input statement (mismatch between @ and identifiers or incorrect format)\n", print_buffer);
}
free(content);
}
printed = -1;
at_count = 0;
arg = 0;
}


\([[:space:]]*[0-9]+[[:space:]]*,[[:space:]]*[0-9]+[[:space:]]*\) {
char temp[100];
strcpy(temp, yytext);

char *num = strtok(temp + 1, ",");
char *base_str = strtok(NULL, ")");

if (num && base_str) {
while (*num == ' ' || *num == '\t') num++;
while (*base_str == ' ' || *base_str == '\t') base_str++;

int base = atoi(base_str);
if (base == 2 || base == 8 || base == 10) {
if (validate_integer(num, base)) {
printf("%s\tInteger Constant\n", yytext);
} else {
printf("%s\tError: Invalid integer for given base\n", yytext);
}
} else {
printf("%s\tError: Invalid base \n", yytext);
}
} else {
printf("%s\tError: Invalid integer constant format\n", yytext);
}
}

\([[:space:]]*print[[:space:]]*,[[:space:]]*char[[:space:]]*\) {
if (in_vardecl) {
printf("(\tSeparator\n");
printf("print\tError: Keyword is used as an identifier\n");
printf("char\tKeyword\n");
printf(")\tSeparator\n");
} else {
REJECT;
}
}

\([[:space:]]*{identifier}[[:space:]]*,[[:space:]]*{identifier}[[:space:]]*\) {
if (in_vardecl) {
printf("(\tSeparator\n");
char temp[100];
strcpy(temp, yytext);
char *id = strtok(temp + 1, ",");

while (*id == ' ' || *id == '\t') id++;

char *end = id + strlen(id) - 1;
while (end > id && (*end == ' ' || *end == '\t')) {
*end = '\0';
end--;
}

if (strcmp(id, "print") == 0) {
printf("%s\tError: Keyword is used as an identifier\n", id);
} else if (!is_declared(id)) {
push_var(id);
printf("%s\tIdentifier\n", id);
} else {
printf("%s\tError: Variable is already declared\n", id);
}

printf(",\tSeparator\n");

char *type = strtok(NULL, ")");
while (*type == ' ' || *type == '\t') type++;
end = type + strlen(type) - 1;
while (end > type && (*end == ' ' || *end == '\t')) {
*end = '\0';
end--;
}

if (strcmp(type, "int") == 0 || strcmp(type, "char") == 0) {
printf("%s\tKeyword\n", type);
} else {
printf("%s\tIdentifier\n", type);
}
printf(")\tSeparator\n");
} else {
REJECT;
}
}


[ \t\n]+ { /* ignore whitespace */ }

[();:,{}@] {
printf("%s\tSeparator\n", yytext);
}

. { printf("%s\tUnrecognized token\n", yytext); }

%%

int main() {
yyin = fopen("input.txt", "r");
if (!yyin) {
printf("Error: Could not open input.txt\n");
return 1;
}
yylex();
fclose(yyin);
return 0;
}