package org.antlr.codegen;

import org.antlr.Tool;
import org.antlr.stringtemplate.StringTemplate;
import org.antlr.tool.Grammar;

public class ELispTarget extends Target {
    public String getTargetCharLiteralFromANTLRCharLiteral(CodeGenerator generator,
                                                           String literal){
        
        return "?" + literal.charAt(1);
	}
}
