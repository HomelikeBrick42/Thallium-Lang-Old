#include "Typedefs.hpp"

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

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
	Array<T> result = (Array<T>) {
		.Data = capacity > 0 ? cast(T*) malloc(capacity * sizeof(T)) : nullptr,
		.Length = 0,
		.Capacity = capacity,
	};
	return result;
}

template<typename T>
void Array_Destroy(Array<T> array) {
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
};

#define StringFromLiteral(s) (String) { .Data = cast(u8*) s, .Length = sizeof(s) - 1 }
#define StringPrintfFormat(s) cast(u32) s.Length, s.Data

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

				while (true) {
					switch (Lexer_GetCurrentChar(lexer)) {
						case '0': case '1': case '2': case '3': case '4':
						case '5': case '6': case '7': case '8': case '9': {
							intValue *= base;
							ASSERT(base <= 10);
							intValue += Lexer_NextChar(lexer) - '0';
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

			default: {
				ASSERT(false);
			} break;
		}
	}

	ASSERT(false);
	return nullptr;
}

enum AstKind : u8 {
	AstKind_Integer,
	AstKind_UnaryOperator,
	AstKind_BinaryOperator,

	AstKind_Count,
};

struct Ast {
	AstKind Kind;
};

struct Ast_Integer : Ast {
	Token* IntToken;
};

Ast_Integer* Ast_Integer_Create(Token* intToken) {
	Ast_Integer* ast = cast(Ast_Integer*) calloc(1, sizeof(*ast));
	*ast = {
		{ .Kind = AstKind_Integer },
		.IntToken = intToken,
	};
	return ast;
}

struct Ast_UnaryOperator : Ast {
	Token* Operator;
	Ast* Operand;
};

Ast_UnaryOperator* Ast_UnaryOperator_Create(Token* _operator, Ast* operand) {
	Ast_UnaryOperator* ast = cast(Ast_UnaryOperator*) calloc(1, sizeof(*ast));
	*ast = {
		{.Kind = AstKind_UnaryOperator },
		.Operator = _operator,
		.Operand = operand,
	};
	return ast;
}

struct Ast_BinaryOperator : Ast {
	Ast* Left;
	Token* Operator;
	Ast* Right;
};

Ast_BinaryOperator* Ast_BinaryOperator_Create(Ast* left, Token* _operator, Ast* right) {
	Ast_BinaryOperator* ast = cast(Ast_BinaryOperator*) calloc(1, sizeof(*ast));
	*ast = {
		{.Kind = AstKind_BinaryOperator },
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

Ast* Parser_ParseExpression(Parser* parser) {
	Ast* Parser_ParseBinaryExpression(Parser * parser, u64 parentPresedence);
	return Parser_ParseBinaryExpression(parser, 0);
}

Ast* Parser_ParsePrimaryExpression(Parser* parser) {
	switch (Parser_GetCurrentToken(parser)->Kind) {
		case TokenKind_Integer: {
			return Ast_Integer_Create(Parser_NextToken(parser));
		} break;

		case TokenKind_OpenParentheses: {
			Parser_NextToken(parser);
			Ast* expression = Parser_ParseExpression(parser);
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

Ast* Parser_ParseBinaryExpression(Parser* parser, u64 parentPresedence) {
	Ast* left;

	u64 unaryOperatorPrecedence = Parser_GetUnaryOperatorPresedence(Parser_GetCurrentToken(parser)->Kind);
	if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence > parentPresedence) {
		Token* _operator = Parser_NextToken(parser);
		Ast* operand = Parser_ParseBinaryExpression(parser, unaryOperatorPrecedence);
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
		Ast* right = Parser_ParseBinaryExpression(parser, presedence);
		left = Ast_BinaryOperator_Create(left, _operator, right);
	}

	return left;
}

int main(int argc, char** argv) {
	String source = StringFromLiteral("-1 + 2 * (3 + +4)");

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

	Ast* ast = Parser_ParseExpression(parser);
	void PrintAst(Ast* ast);
	PrintAst(ast);

	return 0;
}

void PrintAst(Ast* ast) {
	ASSERT(ast);

	switch (ast->Kind) {
		case AstKind_Integer: {
			Ast_Integer* integer = cast(Ast_Integer*) ast;
			ASSERT(integer->IntToken->Kind == TokenKind_Integer);
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
