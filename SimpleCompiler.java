import java.io.*;
import java_cup.runtime.*;


public class Compiler {
    public static void main(String[] args)
        throws IOException // may be thrown by the scanner
    {
        // check for command-line args
        if (args.length != 2) {
            System.err.println("please supply name of file to be parsed " +
			                   "and name of file for unparsed version.");
            System.exit(-1);
        }

        // open input file
        FileReader inFile = null;
        try {
            inFile = new FileReader(args[0]);
        } catch (FileNotFoundException ex) {
            System.err.println("File " + args[0] + " not found.");
            System.exit(-1);
        }

        // open output file
        PrintWriter outFile = null;
        try {
            outFile = new PrintWriter(args[1]);
        } catch (FileNotFoundException ex) {
            System.err.println("File " + args[1] +
                               " could not be opened for writing.");
            System.exit(-1);
        }

        parser P = new parser(new Yylex(inFile));

        Symbol root = null; 

        try {
            root = P.parse();
            //System.out.println ("program parsed correctly.");
        } catch (Exception ex){
            System.err.println("Exception occured during parse: " + ex);
            System.exit(-1);
        }

      //start analyze the ast
      	((ProgramNode)root.value).nameAnalysis();
		if(ErrMsg.getErr() == true){
			System.err.println("Errors occured during name analyze" );
			System.exit(-1);
		} else {
			System.out.println("name analyzed correctly");
		}
	
		((ProgramNode)root.value).typeCheck();
		if(ErrMsg.getErr() == true){
			System.err.println("Errors occured during type check" );
			//System.exit(-1);
		} else {
			System.out.println("type checked correctly");
		}
		

//		//start unparse the ast
//        ((ASTnode)root.value).unparse(outFile, 0);
//        outFile.close();
        
        ((ProgramNode)root.value).generate(outFile);
        outFile.close();
        
        return;
    }
}
