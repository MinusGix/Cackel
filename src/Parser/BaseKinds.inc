#ifndef BASE_KIND
#define BASE_KIND(X) X
#endif

BASE_KIND(IdentifierLiteral)
BASE_KIND(NumberLiteral)
BASE_KIND(BooleanLiteral)

BASE_KIND(AddExpression)
BASE_KIND(SubtractExpression)
BASE_KIND(MultiplyExpression)
BASE_KIND(UnaryPlusExpression)
BASE_KIND(UnaryMinusExpression)
BASE_KIND(FunctionCallExpression)
BASE_KIND(VariableAssignment)
BASE_KIND(IntegerCast)