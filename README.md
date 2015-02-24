# plcinterpreter
Interpreter for a Java/C like language written in a common Lisp dialect, Scheme.

##How to use:
1. Clone repository and 'run' `interpreter.rkt`.

2. Each branch has a different interpreter:
  ####`dev` Branch
    This interpreter works for a language that has variables, assignment statements, mathematical expressions, comparison operators, simple if statements, and return statements like the one below.

     	 ```
     	 var x;
     	 x = 10;
     	 var y = 3 * x * 5;
     	 if (x > y)
     	   return x;
     	 else if (x * x > y)
     	   return x * x;
     	 else if (x * (x + x) > y)
     	   return x * (x + x);
     	 else 
     	   return y - 1;
     	 ```
  ####`loopscope` branch
    This interpreter works for a language like the `dev` branch. In addition, it also handles while loops, break statements, continue statements, and scope (`{}`).

     	 ```
     	 var a = 14;
     	 var b = 3 * a - 7;
     	 if (a < b) {
     	   var temp = a;
     	   a = b;
     	   b = temp;
     	 }
     	 var r = a % b;
     	 while (r != 0) {
     	   a = b;
     	   b = r;
     	   r = a % b;
     	 }
     	 return b;
      	 ```

3. Save the above code (or your own code that matches the rules of the above language) to a text file: `"code.txt"`

4. Give the interpreter the initial instruction: `(interpret "code.txt")`

##Goals
Each improvement iteration will be able to handle a larger and larger subset of the Java language.
