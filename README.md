# plcinterpreter
Interpreter for a Java/C like language written in a common Lisp dialect, Scheme.

##How to use:
1. Clone repository and 'run' `interpreter.scm` (see step 2 for different versions).

2. Each branch has a different interpreter:
  ####`dev`
    This interpreter works for a language that has variables, assignment statements, mathematical expressions, comparison operators, simple if statements, and return statements like the one below.

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
  ---------------------------
  ####`loopscope`
    This interpreter works for a language like the `dev` branch. In addition, it also handles while loops, break statements, continue statements, and scope (`{}`).

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
     	 
  ----------------------------
  ####`functions`
    This interpreter works for a language like the `loopscope` branche. In addition, it also handles funtion definitions (in addition to normal function definitions, recursive functions and functions inside functions are handled as well). Currently, these functions only return boolean or int values.

        function fib(a) {
          if (a == 0)
           return 0;
         else if (a == 1)
           return 1;
         else
           return fib(a-1) + fib(a-2);
        }
        function main() {
          return fib(15);
        }
  

3. Save the above code (or your own code that matches the rules of the above language) to a text file: `"code.txt"`

4. Give the interpreter the initial instruction: `(interpret "code.txt")`

##Goals
Each iteration will be able to handle a larger and larger subset of the Java and C languages. 
