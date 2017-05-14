set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
if version < 600
syntax clear
elseif exists("b:current_syntax")
finish
endif
syn case ignore
syn keyword jclKwd pgm proc class dsn[ame] msgclass space disp contained
syn keyword jclKwd parm member cond msglevel order lrecl recfm unit contained
syn keyword jclKwd sysout outlim blksize region dcb amp contained 
syn keyword jclKwd then shr old new mod catlg rlse delete pass keep contained 
syn keyword jclKwd cyl trk vol retain ser label recorg sysda contained 
syn keyword jclKwd dummy  contained 
syn keyword jclCKwd pgm proc class dsn[ame] msgclass space disp contained
syn keyword jclCKwd parm member cond msglevel order lrecl recfm unit contained
syn keyword jclCKwd sysout outlim blksize region dcb amp contained
syn keyword jclCKwd then shr old new mod catlg rlse delete pass keep contained 
syn keyword jclCKwd cyl trk vol retain ser label recorg sysda contained 
syn keyword jclCKwd dummy  contained 
syn keyword jclPgm idcams iebcopy sort icegener adrdssu ftp rexec contained
syn keyword jclPgm iebgener iefbr14 contained
syn keyword jclCPgm idcams iebcopy sort icegener adrdssu ftp rexec contained
syn keyword jclCPgm iebgener iefbr14 contained
"
" Matches main command and special dd
"
syn match jclMainCommand +^//[^* ]*\s\+EXEC+hs=e-3  contained
syn match jclMainCommand +^//[^* ]*\s\+DD+hs=e-1  contained
syn match jclMainCommand +^//[^* ]*\s\+INCLUDE+hs=e-6 contained
syn match jclMainCommand +^//[^* ]*\s\+JCLLIB+hs=e-5 contained
syn match jclMainCommand +^//[^* ]*\s\+JOB+hs=e-2 contained
syn match jclMainCommand +^//[^* ]*\s\+SET+hs=e-2 contained
syn match jclCMainCommand +^//[^* ]*\s\+EXEC+hs=e-3 contained
syn match jclCMainCommand +^//[^* ]*\s\+DD+hs=e-1 contained 
syn match jclCMainCommand +^//[^* ]*\s\+INCLUDE+hs=e-6 contained
syn match jclCMainCommand +^//[^* ]*\s\+JCLLIB+hs=e-5 contained
syn match jclCMainCommand +^//[^* ]*\s\+JOB+hs=e-2 contained
syn match jclCMainCommand +^//[^* ]*\s\+SET+hs=e-2 contained 
syn match jclCond +^//[^* ]*\s\+ELSE+ contained
syn match jclOperator  "[()]" contained
syn match jclCOperator +[()]+ contained
syn match jclNumber +\<\d\+\>+ contained
syn match jclCNumber +\<\d\+\>+ contained
syn match jclDsn +\(\(\w\{1,8}\.\)\+\w\{1,8}\((\w\{1,8})\)\?\|\(&&\w\{1,8}\)\)+ contained
syn match jclCDsn +\(\(\w\{1,8}\.\)\+\w\{1,8}\((\w\{1,8})\)\?\|\(&&\w\{1,8}\)\)+ contained
syn region  jclDblQuote start=+"+ skip=+[^"]+ end=+"+ contained
syn region  jclSnglQuote start=+'+ skip=+[^']+ end=+'+ contained
syn region  jclCDblQuote start=+"+ skip=+[^"]+ end=+"+ contained
syn region  jclCSnglQuote start=+'+ skip=+[^']+ end=+'+ contained
syn cluster jclConditional contains=jclCMainCommand,jclCIF,jclCData,jclCKwd,jclCond,jclCDblQuote,jclCSnglQuote,jclCComment,jclCOperator,jclCDsn,jclCPgm,jclCNumber
syn region jclIF matchgroup=jclCond start=+^//\w*\s\+IF+ end=+^//\w*\s\+ENDIF+ contains=@jclConditional contained 
syn region jclCIF matchgroup=jclCond start=+^//\w*\s\+IF+ end=+^//\w*\s\+ENDIF+ contains=@jclConditional contained
syn match jclCComment   +^//\*.*$+ contained
syn cluster jclNonConditional contains=jclMainCommand,jclKwd,jclIf,jclOperator,jclDblQuote,jclSnglQuote,jclDsn,jclPgm,jclNumber
" High level matches
syn match jclComment    +^//\*.*$+
"syn match jclData  +^[^/].*$+
syn match jclData   +^\([^/]\|/[^*/]\).*$+
syn match jclStatement  +^//[^*].*$+ transparent contains=@jclNonConditional
syn match jclCData  +^\([^/]\|/[^*/]\).*$+ contained
" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_jcl_syntax_inits")
if version < 508
let did_jcl_syntax_inits = 1
command -nargs=+ HiLink hi link <args>
else
command -nargs=+ HiLink hi def link <args>
endif
" hi Comment guifg=darkgrey
" hi jclIF guibg=white
" hi jclCond guibg=grey guifg=darkblue gui=bold
" hi jclCComm guibg=white guifg=darkred
" hi jclCComment guibg=white guifg=darkgrey 
" "hi jclKwd guifg=darkgreen
" "hi jclCKwd guibg=white guifg=darkgreen 
" hi jclKwd guifg=brown
" hi jclCKwd guibg=white guifg=brown
" hi jclMainCommand   guifg=blue 
" hi jclCMainCommand  guifg=blue guibg=grey
" hi jclData  guifg=violet
" hi jclCData     guifg=violet guibg=white
" hi jclOperator  guifg=darkred
" hi jclCOperator  guifg=darkred guibg=white
" hi jclDsn guifg=darkcyan 
" hi jclCDsn guifg=darkcyan guibg=white 
" 
" Standand colors test :
" 
HiLink jclIF    Normal
HiLink jclCIF   Normal
HiLink jclCond  WarningMsg
HiLink jclCComm Statement
HiLink jclCComment Comment 
HiLink jclKwd Statement
HiLink jclCKwd Statement
HiLink jclMainCommand Type 
HiLink jclCMainCommand WarningMsg
HiLink jclOperator  Operator
HiLink jclCOperator Operator
HiLink jclDsn Normal 
HiLink jclCDsn Normal 
HiLink jclData Special
HiLink jclCData Special
HiLink jclPgm Function
HiLink jclCPgm Function
HiLink jclNumber Number
HiLink jclCNumber Number
HiLink jclDblQuote    jclSnglQuote
HiLink jclSnglQuote String
HiLink jclCDblQuote   jclCSnglQuote
HiLink jclCSnglQuote    String
HiLink jclCIF jclIF
HiLink jclComment       Comment
HiLink jclCComment        Comment
HiLink jclComm      Statement
HiLink jclLabel       Label
syn sync fromstart  " syncronize from start
delcommand HiLink
endif
let b:current_syntax = "jcl"
syn keyword xInstruction a ah al alr ap ar contained
syn keyword xInstruction bal balr bas basm bassm bc bcr bctr bsm bxh bxle contained
syn keyword xInstruction c cds ch cl clc clcl cli clm clr cp cr cs cvb cvd contained
syn keyword xInstruction d dp dr contained
syn keyword xInstruction ed edmk ex contained
syn keyword xInstruction ic icm ipm contained
syn keyword xInstruction l la lcr lh lm lnr lpr lr ltr contained
syn keyword xInstruction m mh mp mr mvc mvcin mvcl mvi mvn mvo mvz contained
syn keyword xInstruction n nc ni nr contained
syn keyword xInstruction o oc oi or contained
syn keyword xInstruction pack contained
syn keyword xInstruction s sh sl sla slda sldl sll slr sp sr sra srda srdl srl srp st stb stc contained
syn keyword xInstruction spm stcm sth stm svc contained
syn keyword xInstruction tm tr trt contained
syn keyword xInstruction unpk contained
syn keyword xInstruction x xc xi xr contained
syn keyword xInstruction zap contained

syn keyword xDirective amode com copy csect contained
syn keyword xDirective dc ds drop dsect contained
syn keyword xDirective eject end entry equ contained
syn keyword xDirective ltorg contained
syn keyword xDirective macro mend mexit mnote contained
syn keyword xDirective org print contained
syn keyword xDirective rmode space start title contained
syn keyword xDirective using contained

syn keyword xBranch b bct be ber bh bhr bl blr bm bmr bne bner bnh contained
syn keyword xBranch bnhr bnl bnlr bnm bnmr contained
syn keyword xBranch bno bnor bnp bnpr bnz bnzr bo bor bor bp bpr contained
syn keyword xBranch br bz bz bzr contained
syn keyword xBranch nop nopr contained

syn keyword xMacro actr ago aif anop chau close contained
syn keyword xMacro dcb contained
syn keyword xMacro gbla gblb gblc get contained
syn keyword xMacro lcla lclb lclc contained
syn keyword xMacro open contained
syn keyword xMacro ppio put contained
syn keyword xMacro read contained
syn keyword xMacro seta setb setc snap contained
syn keyword xMacro write wto wtor wtorpc contained

syn keyword xParameter blksize contained
syn keyword xParameter dcb ddname dsorg contained
syn keyword xParameter eodad gen contained
syn keyword xParameter id contained
syn keyword xParameter lrecl lv contained
syn keyword xParameter macrf contained
syn keyword xParameter nogen contained
syn keyword xParameter pdata contained
syn keyword xParameter recfm record ru contained
syn keyword xParameter storage contained


syn keyword xRegister   r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 contained
syn keyword xOperator   and eq ge gt le lt ne not or contained
syn match   xDelimiter  /[+\-,=.*/]/ contained
syn match   xAttribute  /[iklnst]'/ contained
syn match   xSymbol     /[&][0-9a-z@$#_]\+/ contained
syn match   xSymbol     /[.][0-9a-z@$#_]\+/ contained
syn match   xSymbol     /[a-z@#$][0-9a-z@$#_]\+/ contained
syn match   xSymbol1    /[&][0-9a-z@$#_]\+/ contained
syn match   xString     /[cbdxfhp]\(l[1-9][0-9]*\)\?'\([^']\|''\)*'/ contained contains=xSymbol1
syn match   xString     /[ ,]'\([^']\|''\)*'/ contained contains=xSymbol1
syn match   xString1    /'\([^']\|''\)*'/ contained contains=xSymbol1

syn match xLineComment  /^\(\*.*\|\.\*.*\)\%<73c/
syn match xContinue     /\%72c\S/
syn match xLabel        /^[@#$&.]\?[0-9a-z@$#_]\+/ skipwhite nextgroup=xOperation 
syn match xSpaceAndOp1  /^ \{1,14}/ skipwhite nextgroup=xOperation
syn match xOperation    /[0-9a-z@$#&_]\+/ contained contains=xInstruction,xDirective,xBranch,xMacro skipwhite  nextgroup=xOperand,xOperandStr
syn match xContinueLine /^ \{15}/ nextgroup=xOperand
syn match xOperand      /\((.*)\|[iklnst]'[0-9a-z@$#&_]\+\|[^iklnst ]'\([^']\|''\)*'\|[0-9a-z@$#&_]\+\|[+\-,=.*%!~;:?/]\+\)\+/ contained contains=xAttribute,xParameter,xRegister,xString,xOperator,xDelimiter,xSymbol skipwhite nextgroup=xComment
syn match xOperandStr   /'\([^']\|''\)*'\((.*)\)\?/ contained contains=xString1 skipwhite nextgroup=xComment
syn match xComment      /.*\%<73c/ contained
syn match xTodo         /.*:todo:.*/ 

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_asm_syntax_inits")
    if version < 508
        let did_asm_syntax_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif


    " The default methods for highlighting.  Can be overridden later
    HiLink xLineComment   Comment
    HiLink xComment       Comment
    HiLink xAttribute     Special
    HiLink xRegister      Type
    HiLink xContinue      Number
    HiLink xLabel         Identifier
    HiLink xSymbol        Identifier
    HiLink xSymbol1       Identifier
    HiLink xString        String
    HiLink xString1       String

    HiLink xInstruction   Statement
    HiLink xDirective     Special
    HiLink xBranch        Number
    HiLink xMacro         Macro
    HiLink xOperator      Operator
    HiLink xDelimiter     Operator

    HiLink xParameter     Keyword
    HiLink xTodo          Todo

    delcommand HiLink
endif


let b:current_syntax = "hlasm"
