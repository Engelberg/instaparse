(ns instaparse.abnf
  "This is the context free grammar that recognizes ABNF notation."
  (:use instaparse.cfg))

(def abnf
  "
rulelist = ( rule | (c-wisp* c-nl) )+
rule = rulename defined-as elements c-nl
   (* continues if next line starts with white space *)
rulename = ALPHA (ALPHA | DIGIT | '-')*
defined-as = c-wsp* ('=' | '=/') c-wsp*
   (* basic rules definition and incremental alternatives *)
elements = alternation c-wsp*
c-wsp = WSP | (c-nl WSP)
c-nl = comment | CRLF
   (* comment or newline *)
comment = ';' (WSP | VCHAR)* CRLF
alternation = concatenation (c-wsp* '/' c-wsp* concatenation)
concatenation = repetition (c-wsp+ repetition)
repetition = [repeat] element
repeat = DIGIT+ | (DIGIT* '*' DIGIT*)
element = rulename | group | option | char-val | num-val | prose-val
group = '(' c-wsp* alternation c-wsp* ')'
option = '[' c-wsp* alternation c-wsp* ']'
char-val = DQUOTE (SP | VCHAR)* DQUOTE
num-val = '%' (bin-val | dec-val | hex-val)
bin-val = 'b' BIT+
          [ ('.' BIT+)+ | ('-' 1*BIT) ]
          (* series of contatenated bit values or single ONEOF range *)
dec-val = 'd' DIGIT+
          [ ('.' DIGIT+)+ | ('-' DIGIT+) ]
hex-val = 'x' HEXDIG+
          [ ('.' HEXDIG+)+ | ('-' HEXDIG+) ]
prose-val = '<' (SP / VCHAR)* '>'
          (* bracked string of SP and VCHAR without angles
             prose description, to be used as last resort *)
ALPHA = #'[a-zA-Z]'
BIT = '0' / '1'
CHAR = #'[\u0001-\u007F]
    (* any 7-bit US-ASCII character, excluding NUL *)
CR = %x0D
    (* carriage return *)
CRLF = CR LF?
    (* newline (modified for instaparse) *)
CTL = #'[\u0000-\u001F\u007F]'
DIGIT = #'[0-9]'
DQUOTE = '\"'
HEXDIG = DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
HTAB = '\u0009'
LF = '\u000A'
SP = '\u0020'
VCHAR = #'[\u0021-\u007E]'
   (* visible (printing) characters *)
WSP = SP / HTAB
   (* white space *)
")

(def abnf-parser (build-parser abnf :hiccup))