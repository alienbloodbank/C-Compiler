CSC 254/454 Assignment 8: Compiler - Extra Credits
November 19, 2018
Report

Language: Haskell

Source files:
	Scanner.hs: Source for scanning/tokenizing the C subset language.
	SymbolTable.hs: Source for defining data type for and operations on symbol table.
	ParseTable.hs: Source for defining the FIRST, FOLLOW and PREDICT sets with the list of epsilon productions.
	ProgramLayout.hs: Source for adding function code generation support.
	Evaluator.hs Source for calculating array size at compile time.
	compiler.hs: Source for final code generation using one pass recursive descent parsing and translating.

Console Output:
	Success:
		Translated von Neumann subset C language
	Error: (with its line number)
		Scan Error
		Syntax Error
		Semantic Error:
			- Variable redeclaration
			- Using undeclared variable

Building:
	ghc compiler.hs

Compilation:
	./compiler [input.c]

Compilation Test Cases: (c_tests, a8-compiler)
	Passed:
		ab.c, automaton.c, fibonacci.c, loop_while.c, mandel.c, tax.c, arrays_nofunc.c
	Failed:
		MeaningOfLife.c -> (Semantic Error: line 9: 'res' undeclared (first use in this function))
		arrays_func.c -> (Syntax Error: line 3: expected 'identifier' before *)
                nest_func.c -> (Syntax Error: line 10: expected 'identifier', 'printf', 'scanf', .... before int)  

Note: Compile generated code using -w flag
	- gcc -w out.c
	This is required because when we use GNU's labels as values in our int mem array,
	we get a conversion warning. The pointer to these labels are 64-bit values whereas we are using
	a global mem array of 32-bit integers. This can be fixed by declaring all the registers
	and mem as long, but then the test cases wouldn't work correctly as they are only using 'int'.

Extra Credit Notes/Description:
-	Worked alone on the extra credits.
-	Added support for "Stack Overflow Check", "Array Data" and "Nested Functions" all in one source code.
-	Declared mem size to be 20000.
-	Stack overflow check is done in every function's prologue code and pre-jump code.
	These are the places where the stack size is actually updated during run time.
-	When the stack actually "overflows", the code jumps to a fixed code segment which prints "stack overflow" 
	and the program exits.
-	Arrays cannot be passed around in functions as given in "arrays_func.c". Only their indexed values can be passed
	by value just like the other variables.
-	Size of arrays can at most be a constant expression eg. int arr[3 * (2 + 5)].
-	Nested functions can only be defined with variable declarations before any compute code else compiler will throw
	a syntax error
-	Nested functions are based on lexical scoping rules. Non-local variables will be searched in the lexically
	surrounding scope and so on till the global declarations. If no variable is found the compiler will throw a
	semantic error as shown before and abort.
-	For lexical scoping, I maintain a Map of symbol tables per scope indexed by scope number
	(called "nest" in the program). 
-	An extra book keeping information is added in every activation record that tells the scope number for that
	record. This is useful for searching its symbol table which in turn is useful for traversing
	the static chain.
-	To traverse the static chain at runtime, an extra piece of code is generated during compile time
	whenever a variable is replaced with its actual location in the stack if it exists.

Array Example:

#include <stdio.h>
int add(int a, int b) {
	return a +b;
}
int main() {
	int arr[5 * 6 + (10 + 3)];
        arr[0] = 2;
	arr[1] = 8;
	arr[2] = 4;
	arr[4] = 5;
	arr[10] = 9;
	arr[arr[1] + arr[4]] = 23;
	printf("%d\n", add(arr[10], arr[add(arr[1], arr[arr[arr[0] * (100 - 99)]])]));
}

Expected Output: 
32

Nested Functions example:

#include <stdio.h>
int x;
int main() {
	int A() {
		printf("%d\n",x);
	}
	
    int B() {
        int x;
		int C() {
			printf("%d\n", x);
		}
		x = 400;
		A();
		C();
        return -1;
    }
    x = 500;
	B();
	return 0;
}

Expected Output:
500
400

(Please refer last assignment's README for more details on the compiler)
