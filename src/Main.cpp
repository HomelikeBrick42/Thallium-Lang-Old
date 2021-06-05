#include "Typedefs.hpp"

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include <type_traits>

template<typename T>
struct Array {
	T* Data;
	u64 Length;
	u64 Capacity;

	T& operator[](u64 index) {
		#if !defined(NO_ARRAY_BOUNDS_CHECK)
			ASSERT(index < this->Length);
		#endif
		return this->Data[index];
	}

	T& operator[](s64 index) {
		#if !defined(NO_ARRAY_BOUNDS_CHECK)
			ASSERT(index >= 0 && cast(u64) index < this->Length);
		#endif
		return this->Data[index];
	}
};

template<typename T>
Array<T> Array_Create(u64 capacity = 0) {
	Array<T> result = {
		.Data = capacity > 0 ? cast(T*) malloc(capacity * sizeof(T)) : nullptr,
		.Length = 0,
		.Capacity = capacity,
	};
	return result;
}

template<typename T>
void Array_Destroy(Array<T>& array) {
	free(array.Data);
}

template<typename T>
void Array_Push(Array<T>& array, const T& value) {
	if (array.Length >= array.Capacity) {
		if (array.Capacity == 0) {
			array.Capacity = 1;
		} else {
			array.Capacity *= 2;
		}

		array.Data = cast(T*) realloc(array.Data, array.Capacity * sizeof(T));
	}

	memcpy(&array.Data[array.Length++], &value, sizeof(T));
}

template<typename K>
u64 HashTable_Hash(const K& key) {
	static_assert(!std::is_same<void, void>(), "Please create a hash function for this type");
	return 0;
}

template<typename K, typename V, u64 S = 10>
struct HashTable {
	struct Entry {
		K Key;
		V Value;
	};

	Array<Entry> Buckets[S];
};

template<typename K, typename V, u64 S = 10>
HashTable<K, V, S> HashTable_Create() {
	HashTable<K, V, S> result = {};
	for (u64 i = 0; i < S; i++) {
		result.Buckets[i] = Array_Create<typename HashTable<K, V, S>::Entry>();
	}
	return result;
}

template<typename K, typename V, u64 S>
void HashTable_Destroy(HashTable<K, V, S>& table) {
	for (u64 i = 0; i < S; i++) {
		Array_Destroy(table.Buckets[i]);
	}
}

template<typename K, typename V, u64 S>
b8 HashTable_Add(HashTable<K, V, S>& table, const K& key, const V& value) {
	u64 hash = HashTable_Hash(key) % S;
	Array<typename HashTable<K, V, S>::Entry>& bucket = table.Buckets[hash];

	for (u64 i = 0; i < bucket.Length; i++) {
		if (bucket[i].Key == key) {
			return false;
		}
	}

	Array_Push(bucket, {
		.Key = key,
		.Value = value,
	});

	return true;
}

template<typename K, typename V, u64 S>
V* HashTable_Get(HashTable<K, V, S>& table, const K& key) {
	u64 hash = HashTable_Hash(key) % S;
	Array<typename HashTable<K, V, S>::Entry>& bucket = table.Buckets[hash];

	for (u64 i = 0; i < bucket.Length; i++) {
		if (bucket[i].Key == key) {
			return &bucket[i].Value;
		}
	}

	return nullptr;
}

struct String {
	u8* Data;
	u64 Length;

	u8& operator[](u64 index) {
		#if !defined(NO_ARRAY_BOUNDS_CHECK)
			ASSERT(index < this->Length);
		#endif
		return this->Data[index];
	}

	u8& operator[](s64 index) {
		#if !defined(NO_ARRAY_BOUNDS_CHECK)
			ASSERT(index >= 0 && cast(u64) index < this->Length);
		#endif
		return this->Data[index];
	}

	const u8& operator[](u64 index) const {
		#if !defined(NO_ARRAY_BOUNDS_CHECK)
			ASSERT(index < this->Length);
		#endif
		return this->Data[index];
	}

	const u8& operator[](s64 index) const {
		#if !defined(NO_ARRAY_BOUNDS_CHECK)
			ASSERT(index >= 0 && cast(u64) index < this->Length);
		#endif
		return this->Data[index];
	}
};

#define StringFromLiteral(s) (String) { .Data = cast(u8*) s, .Length = sizeof(s) - 1 }
#define StringPrintfFormat(s) cast(u32) s.Length, s.Data

b8 operator==(const String& a, const String& b) {
	if (a.Length != b.Length) {
		return false;
	}

	if (a.Data == b.Data) {
		return true;
	}

	return memcmp(a.Data, b.Data, a.Length * sizeof(*String::Data)) == 0;
}
b8 operator!=(const String& a, const String& b) {
	return !(a == b);
}

static Array<String> InternedStrings = Array_Create<String>();
String InternString(String oldString) {
	for (u64 i = 0; i < InternedStrings.Length; i++) {
		if (InternedStrings[i] == oldString) {
			return InternedStrings[i];
		}
	}

	String newString = {};
	newString.Data = cast(u8*) malloc(oldString.Length * sizeof(*String::Data));
	memcpy(newString.Data, oldString.Data, oldString.Length * sizeof(*String::Data));
	newString.Length = oldString.Length;

	Array_Push(InternedStrings, newString);
	return newString;
}

template<>
u64 HashTable_Hash<String>(const String& string) {
	u64 hash = 0;
	for (u64 i = 0; i < string.Length; i++) {
		hash += string[i];
		hash *= string[i];
	}
	return hash;
}

enum TokenKind : u8 {
	TokenKind_EndOfInput,
	TokenKind_Identifier,
	TokenKind_Integer,
	TokenKind_Plus,
	TokenKind_Minus,
	TokenKind_Asterisk,
	TokenKind_Slash,
	TokenKind_Percent,
	TokenKind_Semicolon,
	TokenKind_Colon,
	TokenKind_Equals,
	TokenKind_OpenParentheses,
	TokenKind_CloseParentheses,

	TokenKind_Count,
};

String TokenKind_Names[TokenKind_Count] = {
	[TokenKind_EndOfInput] = StringFromLiteral("EndOfInput"),
	[TokenKind_Identifier] = StringFromLiteral("Identifier"),
	[TokenKind_Integer] = StringFromLiteral("Integer"),
	[TokenKind_Plus] = StringFromLiteral("+"),
	[TokenKind_Minus] = StringFromLiteral("-"),
	[TokenKind_Asterisk] = StringFromLiteral("*"),
	[TokenKind_Slash] = StringFromLiteral("/"),
	[TokenKind_Percent] = StringFromLiteral("%"),
	[TokenKind_Semicolon] = StringFromLiteral(";"),
	[TokenKind_Colon] = StringFromLiteral(":"),
	[TokenKind_Equals] = StringFromLiteral("="),
};

struct Token {
	TokenKind Kind;
	u64 Position;
	u64 Line;
	u64 Column;
	u64 Length;

	union {
		String StringValue;
		u64 IntValue;
	};
};

Token* Token_Create(TokenKind kind, u64 position, u64 line, u64 column, u64 length) {
	Token* token = cast(Token*) calloc(1, sizeof(*token));
	*token = (Token){
		.Kind = kind,
		.Position = position,
		.Line = line,
		.Column = column,
		.Length = length,
	};
	return token;
}

Token* Token_CreateInteger(TokenKind kind, u64 position, u64 line, u64 column, u64 length, u64 value) {
	Token* token = cast(Token*) calloc(1, sizeof(*token));
	*token = (Token){
		.Kind = kind,
		.Position = position,
		.Line = line,
		.Column = column,
		.Length = length,
		.IntValue = value,
	};
	return token;
}

Token* Token_CreateString(TokenKind kind, u64 position, u64 line, u64 column, u64 length, String value) {
	Token* token = cast(Token*) calloc(1, sizeof(*token));
	*token = (Token){
		.Kind = kind,
		.Position = position,
		.Line = line,
		.Column = column,
		.Length = length,
		.StringValue = value,
	};
	return token;
}

void Token_Destroy(Token* token) {
	free(token);
}

struct Lexer {
	String Source;
	u64 Position;
	u64 Line;
	u64 Column;
	// TODO: Create error array
};

Lexer* Lexer_Create(String source) {
	Lexer* lexer = cast(Lexer*) calloc(1, sizeof(*lexer));
	*lexer = (Lexer){
		.Source = source,
		.Position = 0,
		.Line = 1,
		.Column = 1,
	};
	return lexer;
}

void Lexer_Destroy(Lexer* lexer) {
	free(lexer);
}

u8 Lexer_PeekChar(Lexer* lexer, s64 offset) {
	s64 index = cast(s64) lexer->Position + offset;
	if (index < 0 || cast(u64) index >= lexer->Source.Length) {
		return '\0';
	}
	return lexer->Source[index];
}

u8 Lexer_GetCurrentChar(Lexer* lexer) {
	return Lexer_PeekChar(lexer, 0);
}

u8 Lexer_NextChar(Lexer* lexer) {
	u8 current = Lexer_GetCurrentChar(lexer);
	if (current == '\n') {
		lexer->Line++;
		lexer->Column = 1;
	} else {
		lexer->Column++;
	}
	lexer->Position++;
	return current;
}

Token* Lexer_NextToken(Lexer* lexer) {
	while (true) {
		u64 start = lexer->Position;
		u64 startLine = lexer->Line;
		u64 startColumn = lexer->Column;
		u64 length = 0;

		auto LexChars = [&](TokenKind kind, u64 charCount) -> Token* {
			for (u64 i = 0; i < charCount; i++) {
				Lexer_NextChar(lexer);
				length++;
			}
			return Token_Create(kind, start, startLine, startColumn, length);
		};

		switch (Lexer_GetCurrentChar(lexer)) {
			case '\0': return LexChars(TokenKind_EndOfInput, 0);
			case '+':  return LexChars(TokenKind_Plus, 1);
			case '-':  return LexChars(TokenKind_Minus, 1);
			case '*':  return LexChars(TokenKind_Asterisk, 1);
			case '/':  return LexChars(TokenKind_Slash, 1);
			case '%':  return LexChars(TokenKind_Percent, 1);
			case ':':  return LexChars(TokenKind_Colon, 1);
			case ';':  return LexChars(TokenKind_Semicolon, 1);
			case '=':  return LexChars(TokenKind_Equals, 1);
			case '(':  return LexChars(TokenKind_OpenParentheses, 1);
			case ')':  return LexChars(TokenKind_CloseParentheses, 1);

			case  ' ': case '\n':
			case '\t': case '\r': {
				Lexer_NextChar(lexer);
			} break;

			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9': {
				u64 base = 10;
				u64 intValue = 0;

				if (Lexer_GetCurrentChar(lexer) == '0') {
					Lexer_NextChar(lexer);
					length++;
					switch (Lexer_GetCurrentChar(lexer)) {
						case 'B': case 'b': {
							base = 2;
							Lexer_NextChar(lexer);
							length++;
						} break;

						case 'X': case 'x': {
							base = 16;
							Lexer_NextChar(lexer);
							length++;
						} break;

						default: {
							base = 8;
						} break;
					}
				}

				while (true) {
					switch (Lexer_GetCurrentChar(lexer)) {
						case '0': case '1': case '2': case '3': case '4':
						case '5': case '6': case '7': case '8': case '9':
						case 'a': case 'b': case 'c': case 'd': case 'e':
						case 'f': case 'g': case 'h': case 'i': case 'j':
						case 'k': case 'l': case 'm': case 'n': case 'o':
						case 'p': case 'q': case 'r': case 's': case 't':
						case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
						case 'A': case 'B': case 'C': case 'D': case 'E':
						case 'F': case 'G': case 'H': case 'I': case 'J':
						case 'K': case 'L': case 'M': case 'N': case 'O':
						case 'P': case 'Q': case 'R': case 'S': case 'T':
						case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z': {
							u8 chr = Lexer_NextChar(lexer);
							u64 value = 0;

							ASSERT(base <= 36);
							if (chr >= 'a' && chr <= 'z') {
								value = cast(u64) chr - 'a' + 10;
							} else if (chr >= 'A' && chr <= 'Z') {
								value = cast(u64) chr - 'A' + 10;
							} else if (chr >= '0' && chr <= '9') {
								value = cast(u64) chr - '0';
							} else {
								ASSERT(false);
							}

							intValue *= base;
							intValue += value;
							length++;
						} continue;

						case '_': {
							Lexer_NextChar(lexer);
							length++;
						} break;

						case '.': {
							ASSERT(false);
						} break;

						default: {
						} break;
					}
					break;
				}

				return Token_CreateInteger(TokenKind_Integer, start, startLine, startColumn, length, intValue);
			} break;

			case 'a': case 'b': case 'c': case 'd': case 'e':
			case 'f': case 'g': case 'h': case 'i': case 'j':
			case 'k': case 'l': case 'm': case 'n': case 'o':
			case 'p': case 'q': case 'r': case 's': case 't':
			case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
			case 'A': case 'B': case 'C': case 'D': case 'E':
			case 'F': case 'G': case 'H': case 'I': case 'J':
			case 'K': case 'L': case 'M': case 'N': case 'O':
			case 'P': case 'Q': case 'R': case 'S': case 'T':
			case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
			case '_': {
				Array<u8> chars = Array_Create<u8>();
				defer(Array_Destroy(chars));

				while (true) {
					switch (Lexer_GetCurrentChar(lexer)) {
						case '0': case '1': case '2': case '3': case '4':
						case '5': case '6': case '7': case '8': case '9':
						case 'a': case 'b': case 'c': case 'd': case 'e':
						case 'f': case 'g': case 'h': case 'i': case 'j':
						case 'k': case 'l': case 'm': case 'n': case 'o':
						case 'p': case 'q': case 'r': case 's': case 't':
						case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
						case 'A': case 'B': case 'C': case 'D': case 'E':
						case 'F': case 'G': case 'H': case 'I': case 'J':
						case 'K': case 'L': case 'M': case 'N': case 'O':
						case 'P': case 'Q': case 'R': case 'S': case 'T':
						case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
						case '_': {
							Array_Push(chars, Lexer_NextChar(lexer));
							length++;
						} continue;

						default: {
						} break;
					}
					break;
				}

				String identifier = InternString({
					.Data = chars.Data,
					.Length = chars.Length,
				});
				return Token_CreateString(TokenKind_Identifier, start, startLine, startColumn, length, identifier);
			} break;

			default: {
				ASSERT(false);
			} break;
		}
	}

	ASSERT(false);
	return nullptr;
}

enum AstKind : u8 {
	AstKind_Assignment,

	AstKind_Identifier,
	AstKind_Integer,
	AstKind_UnaryOperator,
	AstKind_BinaryOperator,

	AstKind_Count,
};

struct Ast {
	AstKind Kind;
};

struct Ast_Expression : Ast {
};

struct Ast_Assignment : Ast {
	Token* Name;
	Ast_Expression* Value;
};

Ast_Assignment* Ast_Assignment_Create(Token* name, Ast_Expression* value) {
	Ast_Assignment* ast = cast(Ast_Assignment*) calloc(1, sizeof(*ast));
	*ast = {
		{ .Kind = AstKind_Assignment },
		.Name = name,
		.Value = value,
	};
	return ast;
}

struct Ast_Identifier : Ast_Expression {
	Token* IdentifierToken;
};

Ast_Identifier* Ast_Identifier_Create(Token* identifierToken) {
	Ast_Identifier* ast = cast(Ast_Identifier*) calloc(1, sizeof(*ast));
	*ast = {
		{ { .Kind = AstKind_Identifier } },
		.IdentifierToken = identifierToken,
	};
	return ast;
}

struct Ast_Integer : Ast_Expression {
	Token* IntToken;
};

Ast_Integer* Ast_Integer_Create(Token* intToken) {
	Ast_Integer* ast = cast(Ast_Integer*) calloc(1, sizeof(*ast));
	*ast = {
		{ { .Kind = AstKind_Integer } },
		.IntToken = intToken,
	};
	return ast;
}

struct Ast_UnaryOperator : Ast_Expression {
	Token* Operator;
	Ast_Expression* Operand;
};

Ast_UnaryOperator* Ast_UnaryOperator_Create(Token* _operator, Ast_Expression* operand) {
	Ast_UnaryOperator* ast = cast(Ast_UnaryOperator*) calloc(1, sizeof(*ast));
	*ast = {
		{ { .Kind = AstKind_UnaryOperator } },
		.Operator = _operator,
		.Operand = operand,
	};
	return ast;
}

struct Ast_BinaryOperator : Ast_Expression {
	Ast_Expression* Left;
	Token* Operator;
	Ast_Expression* Right;
};

Ast_BinaryOperator* Ast_BinaryOperator_Create(Ast_Expression* left, Token* _operator, Ast_Expression* right) {
	Ast_BinaryOperator* ast = cast(Ast_BinaryOperator*) calloc(1, sizeof(*ast));
	*ast = {
		{ { .Kind = AstKind_BinaryOperator } },
		.Left = left,
		.Operator = _operator,
		.Right = right,
	};
	return ast;
}

struct Parser {
	Array<Token*> Tokens;
	u64 Position;
	// TODO: Create error array
};

Parser* Parser_Create(String source) {
	Parser* parser = cast(Parser*) calloc(1, sizeof(*parser));
	*parser = {
		.Tokens = Array_Create<Token*>(),
		.Position = 0,
	};

	Lexer* lexer = Lexer_Create(source);
	defer(Lexer_Destroy(lexer));

	while (true) {
		Token* token = Lexer_NextToken(lexer);
		Array_Push(parser->Tokens, token);

		if (token->Kind == TokenKind_EndOfInput) {
			break;
		}
	}

	// TODO: Lexer errors

	return parser;
}

void Parser_Destroy(Parser* parser) {
	Array_Destroy(parser->Tokens);
	free(parser);
}

Token* Parser_PeekToken(Parser* parser, s64 offset) {
	s64 index = cast(s64) parser->Position + offset;
	if (index < 0 || cast(u64) index >= parser->Tokens.Length) {
		Token* token = parser->Tokens[parser->Tokens.Length - 1];
		ASSERT(token->Kind == TokenKind_EndOfInput);
		return token;
	}
	return parser->Tokens[index];
}

Token* Parser_GetCurrentToken(Parser* parser) {
	return Parser_PeekToken(parser, 0);
}

Token* Parser_NextToken(Parser* parser) {
	Token* current = Parser_GetCurrentToken(parser);
	parser->Position++;
	return current;
}

u64 Parser_GetUnaryOperatorPresedence(TokenKind kind) {
	switch (kind) {
		case TokenKind_Plus:
		case TokenKind_Minus: {
			return 3;
		} break;

		default: {
			return 0;
		} break;
	}

	ASSERT(false);
	return 0;
}

u64 Parser_GetBinaryOperatorPresedence(TokenKind kind) {
	switch (kind) {
		case TokenKind_Asterisk:
		case TokenKind_Slash:
		case TokenKind_Percent: {
			return 2;
		} break;

		case TokenKind_Plus:
		case TokenKind_Minus: {
			return 1;
		} break;

		default: {
			return 0;
		} break;
	}

	ASSERT(false);
	return 0;
}

Ast_Expression* Parser_ParseExpression(Parser* parser) {
	Ast_Expression* Parser_ParseBinaryExpression(Parser * parser, u64 parentPresedence);
	return Parser_ParseBinaryExpression(parser, 0);
}

Ast_Expression* Parser_ParsePrimaryExpression(Parser* parser) {
	switch (Parser_GetCurrentToken(parser)->Kind) {
		case TokenKind_Integer: {
			return Ast_Integer_Create(Parser_NextToken(parser));
		} break;

		case TokenKind_Identifier: {
			return Ast_Identifier_Create(Parser_NextToken(parser));
		} break;

		case TokenKind_OpenParentheses: {
			Parser_NextToken(parser);
			Ast_Expression* expression = Parser_ParseExpression(parser);
			ASSERT(Parser_NextToken(parser)->Kind == TokenKind_CloseParentheses);
			return expression;
		} break;

		default: {
			return Parser_ParseExpression(parser);
		} break;
	}

	ASSERT(false);
	return nullptr;
}

Ast_Expression* Parser_ParseBinaryExpression(Parser* parser, u64 parentPresedence) {
	Ast_Expression* left;

	u64 unaryOperatorPrecedence = Parser_GetUnaryOperatorPresedence(Parser_GetCurrentToken(parser)->Kind);
	if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence > parentPresedence) {
		Token* _operator = Parser_NextToken(parser);
		Ast_Expression* operand = Parser_ParseBinaryExpression(parser, unaryOperatorPrecedence);
		left = Ast_UnaryOperator_Create(_operator, operand);
	}
	else {
		left = Parser_ParsePrimaryExpression(parser);
	}

	while (true) {
		u64 presedence = Parser_GetBinaryOperatorPresedence(Parser_GetCurrentToken(parser)->Kind);
		if (presedence == 0 || presedence <= parentPresedence) {
			break;
		}

		Token* _operator = Parser_NextToken(parser);
		Ast_Expression* right = Parser_ParseBinaryExpression(parser, presedence);
		left = Ast_BinaryOperator_Create(left, _operator, right);
	}

	return left;
}

Array<Ast*> Parser_Parse(Parser* parser) {
	Array<Ast*> statements = Array_Create<Ast*>();

	while (Parser_GetCurrentToken(parser)->Kind != TokenKind_EndOfInput) {
		if (Parser_GetCurrentToken(parser)->Kind == TokenKind_Semicolon) {
			Parser_NextToken(parser);
		} else if (Parser_GetCurrentToken(parser)->Kind == TokenKind_Identifier &&
			Parser_PeekToken(parser, 1)->Kind == TokenKind_Equals) {
			Token* name = Parser_NextToken(parser);
			ASSERT(Parser_NextToken(parser)->Kind == TokenKind_Equals);
			Ast_Expression* expression = Parser_ParseExpression(parser);
			ASSERT(Parser_NextToken(parser)->Kind == TokenKind_Semicolon);
			Array_Push<Ast*>(statements, Ast_Assignment_Create(name, expression));
		} else {
			Ast_Expression* expression = Parser_ParseExpression(parser);
			ASSERT(Parser_NextToken(parser)->Kind == TokenKind_Semicolon);
			Array_Push<Ast*>(statements, expression);
		}
	}

	return statements;
}

int main(int argc, char** argv) {
	String source = StringFromLiteral(R"(
1 + 2 * 3;
hello = 5 + 3 * 3121;
var = hello * 10;
)");

#if 0
	Lexer* lexer = Lexer_Create(source);
	defer (Lexer_Destroy(lexer));

	while (true) {
		Token* token = Lexer_NextToken(lexer);
		defer(Token_Destroy(token));

		printf("Kind: %.*s", StringPrintfFormat(TokenKind_Names[token->Kind]));

		switch (token->Kind) {
			case TokenKind_Integer: {
				printf(", Value: %llu\n", token->IntValue);
			} break;

			default: {
				printf("\n");
			} break;
		}

		if (token->Kind == TokenKind_EndOfInput) {
			break;
		}
	}
#endif

	Parser* parser = Parser_Create(source);
	defer(Parser_Destroy(parser));

	Array<Ast*> statements = Parser_Parse(parser);
	for (u64 i = 0; i < statements.Length; i++) {
		void PrintAst(Ast* ast);
		PrintAst(statements[i]);
		printf("\n");
	}

	return 0;
}

void PrintAst(Ast* ast) {
	ASSERT(ast);

	switch (ast->Kind) {
		case AstKind_Assignment: {
			Ast_Assignment* assignment = cast(Ast_Assignment*) ast;
			printf("%.*s = ", StringPrintfFormat(assignment->Name->StringValue));
			PrintAst(assignment->Value);
		} break;

		case AstKind_Identifier: {
			Ast_Identifier* identifier = cast(Ast_Identifier*) ast;
			printf("%.*s", StringPrintfFormat(identifier->IdentifierToken->StringValue));
		} break;

		case AstKind_Integer: {
			Ast_Integer* integer = cast(Ast_Integer*) ast;
			printf("%llu", integer->IntToken->IntValue);
		} break;

		case AstKind_UnaryOperator: {
			Ast_UnaryOperator* unaryOperator = cast(Ast_UnaryOperator*) ast;
			printf("(");
			printf("%.*s ", StringPrintfFormat(TokenKind_Names[unaryOperator->Operator->Kind]));
			PrintAst(unaryOperator->Operand);
			printf(")");
		} break;

		case AstKind_BinaryOperator: {
			Ast_BinaryOperator* binaryOperator = cast(Ast_BinaryOperator*) ast;
			printf("(");
			PrintAst(binaryOperator->Left);
			printf(" %.*s ", StringPrintfFormat(TokenKind_Names[binaryOperator->Operator->Kind]));
			PrintAst(binaryOperator->Right);
			printf(")");
		} break;

		default: {
			ASSERT(false);
		} break;
	}
}
