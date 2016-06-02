local function INCLUDE (fname, src)
    local f = assert(io.open(fname,'w'))
    f:write(src)
    f:close()
end

----------------------------------------------------------------------------
-- NO: testing
----------------------------------------------------------------------------

--[===[
do return end
--]===]

----------------------------------------------------------------------------
-- OK: well tested
----------------------------------------------------------------------------

Test { [[escape (1);]], run=1 }
Test { [[escape 1;]], run=1 }

Test { [[escape 1; // escape 1;]], run=1 }
Test { [[escape /* */ 1;]], run=1 }
Test { [[escape /*

*/ 1;]], run=1 }
Test { [[escape /**/* **/ 1;]], run=1 }
Test { [[escape /**/* */ 1;]],
    parser = 'line 1 : after `escape´ : expected `escape´ identifier',
}

Test { [[
do do do do do do do do do do do do do do do do do do do do
end end end end end end end end end end end end end end end end end end end end
escape 1;
]],
    run = 1
}

Test { [[
do do
end end
escape 1;
]],
    --ast = 'line 2 : max depth of 127',
    run = 1
}
Test { [[
do do do do do do do do do do do do do do do do do do do do
do do do do do do do do do do do do do do do do do do do do
do do do do do do do do do do do do do do do do do do do do
end end end end end end end end end end end end end end end end end end end end
end end end end end end end end end end end end end end end end end end end end
end end end end end end end end end end end end end end end end end end end end
escape 1;
]],
    --ast = 'line 2 : max depth of 127',
    run = 1
}

Test { [[
do do do do do do do do do do do do do do do do do do do do
do do do do do do do do do do do do do do do do do do do do
do do do do do do do do do do do do do do do do do do do do
do do do do do do do do do do do do do do do do do do do do
do do do do do do do do do do do do do do do do do do do do
do do do do do do do do do do do do do do do do do do do do
end end end end end end end end end end end end end end end end end end end end
end end end end end end end end end end end end end end end end end end end end
end end end end end end end end end end end end end end end end end end end end
end end end end end end end end end end end end end end end end end end end end
end end end end end end end end end end end end end end end end end end end end
end end end end end end end end end end end end end end end end end end end end
escape 1;
]],
    --ast = 'line 5 : max depth of 0xFF',
    run = 1
}

Test { [[escape 0;]], run=0 }
Test { [[escape 9999;]], run=9999 }
Test { [[escape -1;]], run=-1 }
Test { [[escape --1;]], run=1 }
Test { [[escape - -1;]], run=1 }
Test { [[escape -9999;]], run=-9999 }
Test { [[escape 'A';]], run=65, }
Test { [[escape (((1)));]], run=1 }
Test { [[escape 1+2*3;]], run=7 }
Test { [[escape(4/2*3);]], run=6 }
Test { [[escape 2-1;]], run=1 }

Test { [[escape 1==2;]], run=0 }
Test { [[escape 0  or  10;]], run=1 }
Test { [[escape 0 and 10;]], run=0 }
Test { [[escape 2>1 and 10!=0;]], run=1 }
Test { [[escape (1<=2) + (1<2) + 2/1 - 2%3;]], run=2 }
-- TODO: linux gcc only?
--Test { [[escape (~(~0b1010 & 0XF) | 0b0011 ^ 0B0010) & 0xF;]], run=11 }
Test { [[nt a;]],
    parser = 'line 1 : after `nt´ : expected `[´ or `:´ or `.´ or `!´ or `=´ or `:=´ or `;´',
}
Test { [[nt sizeof;]],
    parser = 'line 1 : after `nt´ : expected `[´ or `:´ or `.´ or `!´ or `=´ or `:=´ or `;´',
}
Test { [[var int sizeof;]],
    parser = "line 1 : after `int´ : expected type modifier or internal identifier",
}
Test { [[escape sizeof(int);]], run=4 }
Test { [[escape 1<2>3;]], run=0 }

-->>> NATIVE

Test { [[
_f();
escape 0;
]],
    tops = 'line 1 : native "_f" is not declared',
}

Test { [[
native _f, _f;
escape 0;
]],
    tops = 'line 1 : identifier "_f" is already declared',
}

Test { [[
native _f;
native _f;
escape 0;
]],
    tops = 'line 2 : identifier "_f" is already declared',
}

Test { [[
native _f;
native/end;
native _g;
escape 0;
]],
    tops = 'line 3 : native declarations are disabled',
}

--<<< NATIVE

Test { [[var int a;]],
    ref = 'uninitialized variable "a" crossing compound statement (tests.lua:1)',
}

Test { [[var int a=0;]],
    ana = 'line 1 : missing `escape´ statement for the block',
}

Test { [[var int a=0;]],
    wrn = true,
    _ana = {
        reachs = 1,
        isForever = true,
    }
}

Test { [[
1 = 1;
]],
    parser = 'TODO: exp-lval',
}
Test { [[
_f() = 1;
]],
    parser = 'TODO: exp-lval',
}

Test { [[
native _x;
native do
    int x = 1;
end
escape (_x);
]],
    run = 1,
}
Test { [[
escape (1+1).v;
]],
    env = 'line 1 : not a struct',
}

Test { [[
var int a, b;
a=0; b=0;
escape 10;
]],
    run = 10,
}

Test { [[a = 1; escape a;]],
    locs = 'internal identifier "a" is not declared',
}
Test { [[var int a; a = 1; escape a;]],
    run = 1,
}
Test { [[var int a = 1; escape a;]],
    run = 1,
}
Test { [[var int a = 1; escape (a);]],
    run = 1,
}
Test { [[var int a = 1;]],
    wrn = true,
    _ana = {
        reachs = 1,
        isForever = true,
    }
}
Test { [[var int a=1;var int a=0; escape a;]],
    locs = 'line 1 : declaration of "a" hides previous declaration',
}
Test { [[var int a=1;var int a=0; escape a;]],
    --locs = 'line 1 : internal identifier "a" is already declared at line 1',
    wrn = true,
    run = 0,
}
Test { [[var int b=2; var int a=1; b=a; var int a=0; escape b+a;]],
    wrn = true,
    --locs = 'line 1 : internal identifier "a" is already declared at line 1',
    run = 1,
}
Test { [[do var int a=1; end var int a=0; escape a;]],
    gcc = 'error: variable ‘__ceu_a_1’ set but not used',
    --run = 0,
}
Test { [[var int a=1,a=0; escape a;]],
    wrn = true,
    --locs = 'line 1 : internal identifier "a" is already declared at line 1',
    run = 0,
}
Test { [[var int a; a = b = 1]],
    parser = "line 1 : after `b´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `;´",
}
Test { [[var int a = b; escape 0;]],
    locs = 'internal identifier "b" is not declared',
}
Test { [[escape 1;2;]],
    parser = "line 1 : after `;´ : expected end of file",
}
Test { [[escape 1;2]],
    parser = "line 1 : after `;´ : expected end of file",
}
Test { [[var int aAa; aAa=1; escape aAa;]],
    run = 1,
}
Test { [[var int a; a=1; escape a;]],
    run = 1,
}
Test { [[var int a; a=1; a=2; escape a;]],
    run = 2,
}
Test { [[var int a; a=1; escape a;]],
    run = 1,
}
Test { [[var int a; a=1 ; a=a; escape a;]],
    run = 1,
}
Test { [[var int a; a=1 ; ]],
    wrn = true,
    _ana = {
        reachs = 1,
        isForever = true,
    }
}

-- PRECEDENCE (TODO)
Test { [[
native _assert;
var int v1 = 1 + 1 and 0;    // 0
_assert(v1 == 0);

var int v2 = 1 + 1 or  0;    // 1
_assert(v2 == 1);

var int v3 = 0 and 0 or 1;   // 1
_assert(v3 == 1);

var int v4 = 0 == 0 | 1;     // 0
_assert(v4 == 0);

var int v5 = 0 == 0 & 0;     // 1
_assert(v5 == 1);

escape 1;
]],
    run = 1,
}
Test { [[
inputintMY_EVT;
ifv==0thenbreak;end
]],
    --parser = 'line 2 : after `==´ : expected expression',
    parser = 'line 2 : after `ifv´ : expected `[´ or `:´ or `.´ or `!´ or `;´',
    --parser = 'line 2 : after `0´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `=´ or `:=´ or `;´',
}
Test { [[
inputintMY_EVT;
escape 1;
]],
    locs = 'line 1 : internal identifier "inputintMY_EVT" is not declared',
}

Test { [[
// input event identifiers must be all in uppercase
// 'MY_EVT' is an event of ints
native_printf();
escape 0;
]],
    locs = 'line 3 : internal identifier "native_printf" is not declared',
}

Test { [[
native_printf();
loopdo await250ms;_printf("Hello World!\n");end
]],
    parser = 'line 2 : after `loopdo´ : expected `[´ or `:´ or `.´ or `!´ or `=´ or `:=´ or `;´',
    --parser = 'line 2 : after `loopdo´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `=´ or `:=´ or `;´',
}

-- TYPE / BOOL

Test { [[
input void A;
var bool a? = 1;
a? = 2;
escape a?;
]],
    parser = 'line 2 : after `a´ : expected `=´ or `:=´ or `,´ or `;´',
    --run = 2,
}

Test { [[
input void A;
var bool a = 1;
a = 2;
escape a;
]],
    tops = 'line 1 : input "A declared but not used',
    --run = 2,
}

Test { [[
input void A;
var bool a = 1;
a = 2;
escape a;
]],
    wrn = true,
    run = 2,
}

Test { [[
var bool v = true;
v = false;
if v then
    escape 1;
else
    escape 2;
end
]],
    run = 2,
}

Test { [[
var bool v = false;
v = true;
if v then
    escape 1;
else
    escape 2;
end
]],
    run = 1,
}

-- TYPE / NATIVE / ANNOTATIONS

Test { [[
escape 1;
native do end
]],
    run = 1,
}

Test { [[
native do
    int _ = 3;
end
native/const __;

var int _ = 1;
var int _ = 2;

escape __;
]],
    parser = 'line 6 : after `int´ : expected type modifier or internal identifier',
    --parser = 'line 6 : after `=´ : expected class identifier',
    --env = 'line 6 : invalid access to `_´',
    --locs = 'line 6 : internal identifier "_" is not declared',
    --run = 3,
}
Test { [[
native do
    int _ = 3;
end
native/const __;
native/const _;      // `_´ is special (not C)

var int _ = 1;
var int _ = 2;

escape __;
]],
    parser = 'line 5 : after `const´ : expected native identifier',
    --run = 3,
}
Test { [[
native do
    int _ = 3;
end
native/const __;

var int _;
var int _;
do
    var byte _;
end

escape (int) __;
]],
    parser = 'line 6 : after `int´ : expected type modifier or internal identifier',
    --parser = 'line 6 : after `_´ : expected `with´',
    --run = 3,
}

Test { [[
native _abc; // TODO: = 0;
event void a;
var _abc b;
]],
    env = 'line 3 : cannot instantiate type "_abc"',
}

Test { [[
event u8&& a;  // allowed by compiler

var u8 k = 5;

emit a => &&k; // leads to compiler error
]],
    env = 'line 1 : invalid event type'
}

-->>> OS_START / ANY

Test { [[
native _f,_int;
input void OS_START;
native do
    int f () { escape 1; }
end
var _int x = _f() as /plain;
escape x;
]],
    wrn = true,
    run = 1,
}
Test { [[
input void OS_START;
native _int;
native/pure _f;
native do
    int f () { escape 1; }
end
var _int x = _f();
escape x;
]],
    wrn = true,
    run = 1,
}
Test { [[
input void OS_START;
native _int, _f;
native do
    int f () { escape 1; }
end
var _int x = (_f as /pure)();
escape x;
]],
    wrn = true,
    run = 1,
}
Test { [[
input void OS_START;
native _f;
native do
    void* V;
    int f (void* v) { escape 1; }
end
var void&& ptr = null;
var int x = (_f as/nohold)(ptr);
escape x;
]],
    wrn = true,
    run = 1,
}
Test { [[
input void OS_START;
native _f;
native do
    void* V;
    int f (void* v) { escape 1; }
end
var void&& ptr = null;
var int x = (_f as/pure)(ptr);
escape x;
]],
    wrn = true,
    run = 1,
}

Test { [[
input void ANY;
await ANY;
escape 1;
]],
    run = { ['~>1s']=1 },
}

Test { [[
input void OS_START;
input void A, B;
input void ANY;
var int ret = 0;
await OS_START;
par/or do
    await B;
with
    every ANY do
        ret = ret + 1;
    end
end
escape ret;
]],
    wrn = true,
    run = { ['~>1s;~>A;~>B']=5 },
}

Test { [[
input void ANY;
var int ret = 0;
par/or do
    every ANY do
        ret = ret + 1;
    end
with
    every 1ms do
    end
with
    await 1ms;
end
escape ret;
]],
    run = { ['~>1s']=1001 },
}

--<<< OS_START / ANY

Test { [[
native _abc;
pre native do
    typedef u8  abc;
end
event void a;
var _abc b=0;
escape 1;
]],
    run = 1,
}

Test { [[
native _abc;// TODO = 0;
event void a;
var _abc a;
]],
    wrn = true,
    --locs = 'line 3 : internal identifier "a" is already declared at line 2',
    env = 'line 3 : cannot instantiate type "_abc"',
}

Test { [[
native do
    ##ifndef CEU_EXTS
    ##error bug found
    ##endif
end
escape 1;
]],
    run = 1,
    gcc = 'error: #error bug found',
}

Test { [[
native do
    ##ifndef CEU_EXTS
    ##error bug found
    ##endif
    ##ifdef CEU_WCLOCKS
    ##error bug found
    ##endif
    ##ifdef CEU_INTS
    ##error bug found
    ##endif
    ##ifdef CEU_THREADS
    ##error bug found
    ##endif
    ##ifdef CEU_ORGS
    ##error bug found
    ##endif
    ##ifdef CEU_IFCS
    ##error bug found
    ##endif
    ##ifdef CEU_CLEAR
    ##error bug found
    ##endif
    ##ifdef CEU_PSES
    ##error bug found
    ##endif
    ##ifndef CEU_RET
    ##error bug found
    ##endif
    ##ifdef CEU_LUA
    ##error bug found
    ##endif
    ##ifdef CEU_VECTOR
    ##error bug found
    ##endif
end

input void A;
var int a = 1;
a = 2;
escape a;
]],
    wrn = true,
    run = 2,
}

Test { [[
input void A;
var usize a = 1;
a = 2;
escape a;
]],
    wrn = true,
    run = 2,
}

Test { [[
input void A;
var byte a = 1;
a = 2;
escape a;
]],
    wrn = true,
    run = 2,
}

Test { [[
escape 0x1 + 0X1 + 001;
]],
    run = 3,
}

Test { [[
escape 0x1 + 0X1 + 0a01;
]],
    adjs = 'line 1 : malformed number',
}

Test { [[
escape 1.;
]],
    run = 1,
}

Test { [[
var float x = 1.5;
escape x + 0.5;
]],
    run = 2,
}

Test { [[
var uint x = 1.5;
escape x + 0.5;
]],
    run = 1,
}

Test { [[
var byte x = 1.5;
escape x + 0.5;
]],
    run = 1,
}

Test { [[
var byte x = 255;
escape x + 0.5;
]],
    run = 0,
}

Test { [[
native _ISPOINTER, _MINDIST, _TILESHIFT;

                            if (_ISPOINTER(check) && ((check:x+_MINDIST) >> 
                                _TILESHIFT) == tilex ) then
                                escape 0;
end
]],
    locs = 'line 3 : internal identifier "check" is not declared',
}

    -- INVALID TYPE MODIFIERS

Test { [[
vector int[1][1] v;
escape 1;
]],
    --adj = 'line 1 : not implemented : multiple `[]´',
    --env = 'line 1 : invalid type modifier : `[][]´',
    parser = 'line 1 : after `vector´ : expected `&´ or `[´',
}
Test { [[
vector[1][1] int v;
escape 1;
]],
    --adj = 'line 1 : not implemented : multiple `[]´',
    --env = 'line 1 : invalid type modifier : `[][]´',
    parser = 'line 1 : after `]´ : expected type',
}
Test { [[
vector[1] int? v;
escape 1;
]],
    env = 'line 1 : `data´ fields do not support vectors yet',
    --env = 'line 1 : invalid type modifier : `[]?´',
}
Test { [[
var int* v;
escape 1;
]],
    parser = 'after `int´ : expected type modifier or internal identifier',
}
Test { [[
var& int&& v;
escape 1;
]],
    env = 'TODO: uninit',
}
Test { [[
var& int&&  v;
escape 1;
]],
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:1)',
}
Test { [[
var int&& p = null;
var& int&&  v = &p;
escape 1;
]],
    run = 1,
}
Test { [[
vector&[] int v;
escape 1;
]],
    env = 'line 1 : invalid type modifier : `&[]´',
}
Test { [[
var& int&  v;
escape 1;
]],
    parser = 'line 1 : after `int´ : expected type modifier or internal identifier',
    --env = 'line 1 : invalid type modifier : `&&´',
}
Test { [[
var int?&& v;
escape 1;
]],
    parser = 'line 1 : after `?´ : expected internal identifier',
    --env = 'line 1 : invalid type modifier : `?&&´',
    --adj = 'line 1 : not implemented : `?´ must be last modifier',
}
Test { [[
vector[1] int? v;
escape 1;
]],
    run = 1,
    --env = 'line 1 : invalid type modifier : `?[]´',
    --adj = 'line 1 : not implemented : `?´ must be last modifier',
}
Test { [[
var& int? v;
escape 1;
]],
    env = 'line 1 : invalid type modifier : `?&´',
    --adj = 'line 1 : not implemented : `?´ must be last modifier',
}
Test { [[
var int?? v;
escape 1;
]],
    parser = 'line 1 : after `?´ : expected internal identifier',
    --env = 'line 1 : invalid type modifier : `??´',
    --adj = 'line 1 : not implemented : `?´ must be last modifier',
}

    -- IF

Test { [[if 1 then escape 1; end; escape 0;]],
    parser = 'TODO: exp-bool',
    _ana = {
        isForever = false,
    },
    run = 1,
}
Test { [[if false then escape 0; end  escape 1;]],
    run = 1,
}
Test { [[if false then escape 0; else escape 1; end]],
    _ana = {
        isForever = false,
    },
    run = 1,
}
Test { [[if (false) then escape 0; else escape 1; end;]],
    run = 1,
}
Test { [[if (true) then escape (1); end]],
    _ana = {
        reachs = 1,
    },
    run = 1,
}
Test { [[
if (false) then
    escape 1;
end
escape 0;
]],
    run = 0,
}
Test { [[
var int a = 1;
if a == 0 then
    escape 1;
else/if a > 0 then
    escape 0;
else
    escape 1;
end
escape 0;
]],
    _ana = {
        unreachs = 1,
    },
    run = 0,
}
Test { [[
var int a = 1;
if a == 0 then
    escape 0;
else/if a < 0 then
    escape 0;
else
    a = a + 2;
    if a < 0 then
        escape 0;
    else/if a > 1 then
        escape 1;
    else
        escape 0;
    end
    escape 1;
end
escape 0;
]],
    _ana = {
        unreachs = 2,
    },
    run = 1,
}
Test { [[if (true) then  else escape 0; end;]],
    _ana = {
        reachs = 1,
    },
    run = '1] runtime error: missing `escape´ statement',
}

-- IF vs Seq priority
Test { [[if true then var int a=0; if a then end; escape 2; else escape 3; end;]],
    run = 2,
}

Test { [[
if false then
    escape 1;
else
    if true then
        escape 1;
    end
end;]],
    _ana = {
        reachs = 1,
    },
    run = 1,
}
Test { [[
if false then
    escape 1;
else
    if false then
        escape 1;
    else
        escape 2;
    end
end;]],
    _ana = {
        isForever = false,
    },
    run = 2,
}
Test { [[
var int a = 0;
var int b = a;
if b then
    escape 1;
else
    escape 2;
end;
]],
    run = 2,
}
Test { [[
var int a;
if false then
    a = 1;
else
    a = 2;
end;
escape a;
]],
    run = 2,
}
Test { [[
var int a;
var int c = 0;
if false then
    a = 1;
    a = 1;
else
    a = 2;
    c = a;
end;
escape a+c;
]],
    ref = 'line 5 : invalid extra access to variable "a" inside the initializing `if-then-else´',
}
Test { [[
var int a;
var int c = 0;
if false then
    a = 1;
    c = a;
else
    a = 2;
    c = a;
end;
escape a+c;
]],
    run = 4,
    --ref = 'line 5 : invalid access to uninitialized variable "a"',
}
Test { [[
var int a;
var int c = 0;
if false then
    c = a;
    a = 1;
else
    a = 2;
    c = a;
end;
escape a+c;
]],
    ref = 'line 4 : invalid access to uninitialized variable "a"',
}
Test { [[
var int a;
if true then
    a = 1;
    if true then
        a = 2;
    end;
end;
escape a;
]],
    ref = 'line 5 : missing initialization for variable "a" in the other branch of the `if-then-else´',
}
Test { [[
var int a;
if true then
    if true then
        a = 2;
    else
        a = 0;
    end;
else
    a = 10;
end;
escape a;
]],
    run = 2,
}
Test { [[
var int a;
if true then
    a = 1;
    if true then
        a = 2;
    else
        a = 0;
    end;
else
    a = 10;
end;
escape a;
]],
    run = 2,
}
Test { [[
var int a;
if true then
    a = 1;
    if true then
        a = 2;
    else
        a = 0;
    end;
end;
escape a;
]],
    ref = 'line 3 : missing initialization for variable "a" in the other branch of the `if-then-else´ (tests.lua:2)',
}
Test { [[
var int a;
if false then
    escape 1;
else
    a=1;a=2; escape 3;
end;
]],
    ref = 'line 5 : invalid extra access to variable "a" inside the initializing `if-then-else´ (tests.lua:2)',
}
Test { [[
var int a=1;
if false then
    escape 1;
else
    a=1;a=2; escape 3;
end;
]],
    run = 3,
}
Test { [[
var int a;
if false then
    a = 1;
    escape 1;
else
    a=2; escape 3;
end;
]],
    run = 3,
}
Test { [[
var int a = 0;
if (false) then
    a = 1;
end
escape a;
]],
    run = 0,
}

    -- EVENTS

Test { [[input int A=1;
]],
    parser="line 1 : after `A´ : expected `,´ or `;´"
}

Test { [[input int A=1;]],
    parser="line 1 : after `A´ : expected `,´ or `;´"
}

Test { [[
input int A;


A=1;
]],
    parser = 'line 1 : after `;´ : expected statement',
    --parser = 'line 4 : after `A´ : expected `(´',
}

Test { [[
input int A;
A=1;
escape 1;
]],
    --adj = 'line 2 : invalid expression',
    parser = 'line 1 : after `;´ : expected statement',
    --parser = 'line 2 : after `A´ : expected `(´',
    --parser = 'line 1 : after `;´ : expected statement (usually a missing `var´ or C prefix `_´)',
}

Test { [[input  int A;]],
    _ana = {
        reachs = 1,
        isForever = true,
    },
}
Test { [[input int A,A; escape 0;]],
    tops = 'line 1 : identifier "A" is already declared (tests.lua : line 1)',
    --tops = 'external "A" is already declared',
    run = 0,
}
Test { [[
input int A,B,Z;
]],
    _ana = {
        reachs = 1,
        isForever = true,
    },
}

Test { [[await A; escape 0;]],
    tops = 'external "A" is not declared',
}

Test { [[
par/or do
with
end
escape 1;
]],
    run = 1,
}
Test { [[
input void A;
par/or do
    await A;
with
    async do
        emit A;
    end
end
escape 1;
]],
    run = 1,
}
Test { [[
input void A;
await A;
escape 1;
]],
    run = { ['~>A']=1 },
}
Test { [[
input void A;
par/and do
    await A;
with
    nothing;
end
escape 1;
]],
    run = { ['~>A']=1 },
}

Test { [[
input int A;
par/or do
    await A;
with
    async do
        emit A=>10;
    end
end;
escape 10;
]],
    _ana = {
        isForever = false,
    },
    run = 10,
}
Test { [[
input int A;
var int ret=0;
par/or do
    ret = await A;
with
    async do
        emit A => 10;
    end;
end
escape ret;
]],
    run = 10
}

Test { [[
input int A;
var int ret;
par/or do
    ret = await A;
with
    async do
        emit A => 10;
    end;
end
escape ret;
]],
    ref = 'line 2 : uninitialized variable "ret" crossing compound statement (tests.lua:3)',
}

Test { [[
input int A;
par/and do
    await A;
with
    async do
        emit A => 10;
    end;
end;
escape A;
]],
    parser = "line 9 : after `escape´ : expected expression",
    --parser = 'line 9 : after `A´ : expected `(´',
    --adj = 'line 9 : invalid expression',
}

Test { [[
input int A;
var int v=0;
par/and do
    v = await A;
with
    async do
        emit A => 10;
    end;
end;
escape v;
]],
    _ana = {
        isForever = false,
    },
    run = 10,
}

Test { [[
input int A;
var int v = await A;
escape v;
]],
    run = {
        ['101~>A'] = 101,
        ['303~>A'] = 303,
    },
}

Test { [[var int a = a+1; escape a;]],
    --locs = 'internal identifier "a" is not declared',
    todo = 'TODO: deveria dar erro!',
    run = 1,
}

Test { [[var int a; a = emit a => 1; escape a;]],
    --parser = 'line 1 : after `=´ : expected expression',
    parser = "line 1 : after `emit´ : expected number or `(´ or external identifier",
    --trig_wo = 1,
}

Test { [[var int a; emit a => 1; escape a;]],
    env = 'line 1 : identifier "a" is not an event (tests.lua : line 1)',
    --trig_wo = 1,
}
Test { [[event int a=0; emit a => 1; escape a;]],
    env = 'TODO: a=0',
    --parser = 'line 1 : after `a´ : expected `;´',
    --trig_wo = 1,
}
Test { [[
event int a;
emit a => 1;
escape a;
]],
    env = 'line 3 : types mismatch (`int´ <= `void´)',
    --run = 1,
    --trig_wo = 1,
}

Test { [[
event void e;
emit e;
escape 10;
]],
    wrn = true,
    run = 10,
}

Test { [[
var int a=10;
do
    var int b=1;
    if b then end;
end
escape a;
]],
    run = 10,
}

-- TODO: XXX
Test { [[
input void OS_START;
do
    var int v = 0;
    if v then end;
end
event void e;
var int ret = 0;
par/or do
    await OS_START;
    emit e;
    ret = 1;
with
    await e;
    ret = 2;
end
escape ret;
]],
    _ana = {
        excpt = 1,
        --unreachs = 1,
    },
    run = 2,
}

Test { [[
input void OS_START;
var int ret = 0;
par/and do
    await OS_START;
with
    event void e;
    par/or do
        await OS_START;
        emit e;
        ret = 1;
    with
        await e;
        ret = 2;
    end
end
escape ret;
]],
    run = 2,
}

Test { [[
input void OS_START;
do
    var int v = 0;
    if v then end;
end
event void e;
par do
    await OS_START;
    emit e;
    escape 1;       // 9
with
    await e;
    escape 2;       // 12
end
]],
    _ana = {
        excpt = 1,
        --unreachs = 1,
    },
    run = 2,
}
Test { [[
input void OS_START;
do
    var int v = 0;
    if v then end;
end
event void e;
par do
    await OS_START;
    emit e;
    escape 1;       // 9
with
    await e;
    escape 2;       // 12
end
]],
    safety = 2,
    _ana = {
        acc   = 1,
        excpt = 1,
        --unreachs = 1,
    },
    run = 2,
}

Test { [[
input void OS_START;
event void a,b;
par do
    await OS_START;
    emit a;
    escape 10;
with
    await a;
    emit b;
    escape 100;
end
]],
    run = 100;
}

    -- WALL-CLOCK TIME / WCLOCK

Test { [[await 0ms; escape 0;]],
    sval = 'line 1 : constant is out of range',
}
Test { [[
input void A;
await A;
escape 0;
]],
    run = { ['~>10ms; ~>A'] = 0 }
}

Test { [[await -1ms; escape 0;]],
    --ast = "line 1 : after `await´ : expected event",
    --parser = 'line 1 : after `1´ : expected `;´',
    --parser = 'line 1 : after `1´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `until´ or `;´',
    parser = 'line 1 : after `-´ : expected expression',
}

Test { [[await 1; escape 0;]],
    parser = 'line 1 : after `1´ : expected <h,min,s,ms,us>',
}
Test { [[await -1; escape 0;]],
    env = 'line 1 : event "?" is not declared',
}

Test { [[var s32 a=await 10s; escape a==8000000;]],
    _ana = {
        isForever = false,
    },
    run = {
        ['~>10s'] = 0,
        ['~>9s ; ~>9s'] = 1,
    },
}

Test { [[await FOREVER;]],
    _ana = {
        isForever = true,
    },
}

-- tests var.isTmp
Test { [[
native ___ceu_a_1;
var int a = await 999ms;
escape a + ___ceu_a_1;
]],
    run = { ['~>1s']=2000 },
}

Test { [[await FOREVER; await FOREVER;]],
    parser = "line 1 : after `;´ : expected end of file",
}
Test { [[await FOREVER; escape 0;]],
    parser = "line 1 : after `;´ : expected end of file",
}

Test { [[emit 1ms; escape 0;]],
    props = 'invalid `emit´'
}

Test { [[
var int a;
a = async do
    emit 1min;
end;
escape a + 1;
]],
    todo = 'async nao termina',
    run = false,
}

Test { [[
async do
end
escape 10;
]],
    _ana = {
        isForever = false,
    },
    run = 10,
}

Test { [[
var int a = 0;
async do
    emit 1min;
    escape 10;
end;
escape a + 1;
]],
    --locs = 'line 1 : internal identifier "_ret" is not declared',
    --props = 'line 4 : not permitted inside `async´',
    props = 'line 4 : not permitted across `async´ declaration',
}

Test { [[
var int a;
var& int pa = &a;
async (pa) do
    emit 1min;
    pa = 10;
end;
escape a + 1;
]],
    ref = ' line 2 : invalid access to uninitialized variable "a"',
}

Test { [[
var int a = 0;
var& int pa = &a;
async (pa) do
    emit 1min;
    pa = 10;
end;
escape a + 1;
]],
    run = 11,
}

-- Seq

Test { [[
input int A;
var int v = await A;
escape v;
]],
    run = { ['10~>A']=10 },
}
Test { [[
input int A,B;
await A;
var int v = await B;
escape v;
]],
    run = {
        ['3~>A ; 1~>B'] = 1,
        ['1~>B ; 2~>A ; 3~>B'] = 3,
    }
}
Test { [[
var int a = await 10ms;
a = await 20ms;
escape a;
]],
    run = {
        ['~>20ms ; ~>11ms'] = 1000,
        ['~>20ms ; ~>20ms'] = 10000,
    }
}
Test { [[
var int a = await 10us;
a = await 40us;
escape a;
]],
    run = {
        ['~>20us ; ~>30us'] = 0,
        ['~>30us ; ~>10us ; ~>10us'] = 0,
        ['~>30us ; ~>10us ; ~>30us'] = 20,
    }
}

Test { [[
par/and do
    await 1s;
with
    await 1s;
end
par/and do
    await FOREVER;
with
    await 1s;
end
escape 0;
]],
    _ana = {
        unreachs = 2,
        isForever = true,
    },
}

Test { [[
var int ret=0;
par/or do
    ret = 1;
with
    ret = 2;
end
escape ret;
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}

Test { [[
var int ret;
par/or do
    ret = 1;
with
    ret = 2;
end
escape ret;
]],
    ref = 'line 1 : uninitialized variable "ret" crossing compound statement (tests.lua:2)',
}

Test { [[
var int ret=0;
par/or do
    await FOREVER;
with
    ret = 2;
end
escape ret;
]],
    run = 2,
}

Test { [[
var int ret=0;
par/or do
    par/or do
        await FOREVER;
    with
        ret = 1;
    end
with
    ret = 2;
end
escape ret;
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}

Test { [[
var int ret=0;
par/or do
    ret = 2;
with
    par/or do
        await FOREVER;
    with
        ret = 1;
    end
end
escape ret;
]],
    _ana = {
        acc = 1,
    },
    run = 2,
}

Test { [[
var int ret=0;
par/or do
    await FOREVER;
with
    par/or do
        await FOREVER;
    with
        ret = 1;
    end
end
escape ret;
]],
    run = 1,
}

Test { [[
input void B;
var int ret = 0;
par/or do
    await 2s;   // 4
    ret = 10;
    await B;    // 6
with
    await 1s;   // 8
    ret = 1;
    await B;    // 10
end
escape ret;
]],
    _ana = {
        acc = 1,  -- false positive
        abrt = 3,
    },
    run = { ['~>1s; ~>B']=1 },
}

Test { [[
par/or do
    await 1s;       // 2
with
    await 1s;       // 4
end
par/or do
    await 1s;
with
    await FOREVER;
end
par/or do
    await FOREVER;
with
    await FOREVER;
end
escape 0;
]],
    _ana = {
        unreachs = 2,
        isForever =  true,
        abrt = 3,
    },
}

Test { [[
par do
    await FOREVER;
with
    await 1s;
end
]],
    _ana = {
        isForever = true,
    }
}

Test { [[
par do
    await FOREVER;
with
    await FOREVER;
end
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
par do
    await 1s;
with
    await 1s;
end
]],
    _ana = {
        reachs = 1,
        isForever = true,
    },
}

Test { [[
par do
    await 1s;
    await 1s;
    escape 1;   // 4
with
    await 2s;
    escape 2;   // 7
end
]],
    _ana = {
        acc = 1,
        abrt = 4,
    },
    run = { ['~>2s']=1 }
}

Test { [[
par do
    var int v1=4,v2=4;
    par/or do
        await 10ms;
        v1 = 1;
    with
        await 10ms;
        v2 = 2;
    end
    escape v1 + v2;
with
    async do
        emit 5ms;
        emit(5000)ms;
    end
end
]],
    _ana = {
        isForever = false,
        abrt = 3,
    },
    run = 5,
    --run = 3,
    --todo = 'nd excpt',
}

Test { [[
input int A;
await A;
await A;
var int v = await A;
escape v;
]],
    run  = {
        ['1~>A ; 2~>A ; 3~>A'] = 3,
    },
}

Test { [[
input int A,B;
var int ret;
if true then
    ret = await A;
else
    ret = await B;
end;
escape ret;
]],
    run = {
        ['1~>A ; 0~>A'] = 1,
        ['3~>B ; 0~>A'] = 0,
    },
}

Test { [[
input int A;
var int v;
if true then
    v = await A;
else
    v = 0;
end;
escape v;
]],
    run = {
        ['1~>A ; 0~>A'] = 1,
    },
}

Test { [[
input int A;
var int v;
if true then
    v = await A;
end;
escape v;
]],
    ref = 'line 4 : missing initialization for variable "v" in the other branch of the `if-then-else´ (tests.lua:3)',
}

Test { [[
input int A;
var int v = 0;
if false then
    v = await A;
end;
escape v;
]],
    run = 0,
}

Test { [[
par/or do
    await FOREVER;
with
    escape 1;
end
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = 1,
}

-->>> DO/_, SETBLOCK, ESCAPE

Test { [[
do/
    escape/A 1;
end
]],
    parser = 'line 1 : after `do´ : expected `escape´ identifier',
}

Test { [[
do/A
    escape/ 1;
end
]],
    parser = 'line 2 : after `escape´ : expected `escape´ identifier',
}

Test { [[
do/A
    escape/A 1;
end
]],
    env = 'do/A has no escape value',
}

Test { [[
do/A
    escape/A;
end
escape 1;
]],
    run = 1,
}

Test { [[
do/A
    escape;
end
]],
    env = 'missing value',
}

Test { [[
do/A
    escape 1;
end
]],
    run = 1,
}

Test { [[
var int a = do/A
    escape/A 1;
end;
escape 1;
]],
    run = 1,
}

Test { [[
var int a = do/_
    var int a = do/A
        escape/A 1;
    end;
    escape a;
end;
escape a;
]],
    wrn = true,
    run = 1
}

Test { [[
var int a = do/B
    var int a = do/A
        escape/A 1;
    end;
    escape a;
end;
escape a;
]],
    wrn = true,
    run = 1
}

Test { [[
var int a = do/B
    var int a = do/A
        escape/B 1;
    end;
    escape/B 10;
end;
escape a;
]],
    wrn = true,
    run = 1
}

Test { [[
var int a = do/B
    var int a = do/A
        escape/B 1;
    end;
    escape/A a;
end;
escape a;
]],
    locs = 'line 2 : declaration of "a" hides previous declaration (tests.lua : line 1)',
}

Test { [[
var int a = do/B
    var int a = do/A
        escape/B 1;
    end;
    escape/A a;
end;
escape a;
]],
    wrn = true,
    env = 'A not defined',
}

Test { [[
event int aa;
var int a=0;
par/and do
    a = do/_
        escape 1;
    end;
with
    await aa;
end;
escape 0;
]],
    _ana = {
        --unreachs = 2,
        --isForever = true,
    },
}

Test { [[
event int a;
par/and do
    a = do/_
        escape 1;
    end;
with
    await a;
end;
escape 0;
]],
    env = 'line 4 : types mismatch (`void´ <= `int´)',
}

Test { [[
var u8&& ptr =
    par do
        //_idle();
        await FOREVER;
    with
        await 1s;
        escape null;
    end;
escape ptr == null;
]],
    parser = 'line 1 : after `=´ : expected expression',
}
Test { [[
var u8&& ptr = do/_
    par do
        //_idle();
        await FOREVER;
    with
        await 1s;
        escape null;
end
    end;
escape ptr == null;
]],
    run = {['~>1s']=1},
}
Test { [[
var int ret =
    do/_
        if true then
            escape 1;
        end
        escape 0;
    end;
escape ret;
]],
    run = 1,
}

Test { [[
a = do end;
]],
    parser = 'line 1 : after `do´ : expected `/´',
}

Test { [[
a = do/X end;
]],
    locs = 'line 1 : internal identifier "a" is not declared',
}

Test { [[
var int a = do/X end;
]],
    run = 'assertion',
}

--<<< DO/_, SETBLOCK, ESCAPE

Test { [[
input void A,B;
par/or do
    await A;
    await FOREVER;
with
    await B;
    escape 1;
end;
]],
    _ana = {
        unreachs = 1,
    },
    run = { ['~>A;~>B']=1, },
}

Test { [[
par/and do
with
    escape 1;
end
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = 1,
}
Test { [[
par do
with
    escape 1;
end
]],
    run = 1,
    _ana = {
        abrt = 1,
    },
}
Test { [[
par do
    await 10ms;
with
    escape 1;
end
]],
    _ana = {
        abrt = 1,
        --unreachs = 1,
    },
    run = 1,
}
Test { [[
input int A;
par do
    async do end
with
    await A;
    escape 1;
end
]],
    run = { ['1~>A']=1 },
}

Test { [[
par do
    async do end
with
    escape 1;
end
]],
    todo = 'async dos not exec',
    _ana = {
        --unreachs = 1,
    },
    run = 1,
}

Test { [[
par do
    await FOREVER;
with
    escape 1;
end;
]],
    run = 1,
    _ana = {
        abrt = 1,
    },
}

Test { [[
input void A,B;
par do
    await A;
    await FOREVER;
with
    await B;
    escape 1;
end;
]],
    run = { ['~>A;~>B']=1, },
}

-- testa ParOr que da clean em ParOr que ja terminou
Test { [[
input int A,B,C;
par/or do
    await A;
with
    await B;
end;

var int a=0;
par/or do
    a = 255+255+3;
with
    await C;
end;
escape a;
]],
    _ana = {
        --unreachs = 1,
        abrt = 1,
    },
    run = { ['1~>A;1~>C']=513, ['2~>B;0~>C']=513 },
}

Test { [[
input int A,B,C;
par/or do
    await A;
with
    await B;
end;

var int a;
par/or do
    a = 255+255+3;
with
    await C;
end;
escape a;
]],
    ref = 'line 8 : uninitialized variable "a" crossing compound statement (tests.lua:9)',
}

Test { [[
input int A,B,C;
var int a=0;
par/or do
    par/or do
        par/or do
            a = await 10us;
        with
            await A;
        end;
    with
        a = await B;
    end;
    await FOREVER;
with
    await C;
end;
escape a;
]],
    run = {
        ['1~>B; ~>20us; 1~>C'] = 1,
        ['~>20us; 5~>B; 2~>C'] = 10,
    }
}
Test { [[
var int a =
    do/_
        escape 1;
    end;
escape a;
]],
    run = 1,
}
Test { [[
var int a =
    do/_
        escape a;
    end;
escape a;
]],
    ref = 'line 3 : invalid access to uninitialized variable "a" (declared at tests.lua:1)',
}
Test { [[
var int a =
    do/_
        a = 1;
        escape a;
    end;
escape a;
]],
    --ref = 'line 4 : invalid access to uninitialized variable "a" (declared at tests.lua:1)',
    run = 1,
}
Test { [[
var int a = do/_ par do
                escape 1;
            with
            end;
end;
escape a;
]],
    run = 1,
}

Test { [[
var int a = do/_ par do
                escape a;
            with
end
            end;
escape a;
]],
    ref = 'line 2 : invalid access to uninitialized variable "a" (declared at tests.lua:1)',
}

Test { [[
var int a = do/_
    par do
        escape 1;
    with
    end;
end;
escape a;
]],
    run = 1,
}

Test { [[
var int a = do/_
    par/or do
        escape 1;
    with
    end;
    escape 0;
end;
escape a;
]],
    run = 1,
}

Test { [[
input int A,B,C;
var int a = do/_
        par/or do
            par do
                var int v=0;
                par/or do
                    var int v = await 10ms;
                    escape v; //  8
                with
                    v = await A;
                end;
                escape v;     // 12
            with
                var int v = await B;
                escape v;     // 15
            end;
        with
            await C;
        end;
        escape 0;
    end;
escape a;
]],
    wrn = true,
    run = {
        ['1~>B; ~>20ms; 1~>C'] = 1,
        ['~>20ms; 5~>B; 2~>C'] = 10000,
    }
}

Test { [[
input int A,B,C;
var int a = do/_
        par/or do
            par do
                var int v=0;
                par/or do
                    var int v = await 10ms;
                    escape v; //  8
                with
                    v = await A;
                end;
                escape v;     // 12
            with
                var int v = await B;
                escape v;     // 15
            end;
        with
            await C;
        end;
        escape 0;
    end;
escape a;
]],
    wrn = true,
    run = {
        ['1~>B; ~>20ms; 1~>C'] = 1,
        ['~>20ms; 5~>B; 2~>C'] = 10000,
    },
    safety = 2,
    _ana = {
        acc = 2,
    },
}

Test { [[
input int A,B,C;
var int a = do/_
        par/or do
            par do
                var int v;
                par/or do
                    var int v = await 10ms;
                    escape v;
                with
                    v = await A;
                end;
                escape v;
            with
                var int v = await B;
                escape v;
            end;
            // unreachable
            await FOREVER;
        with
            await C;
        end;
        escape 0;
    end;
escape a;
]],
    -- TODO: melhor seria: unexpected statement
    parser = "line 16 : after `;´ : expected `with´",
    --unreachs = 1,
    run = {
        ['1~>B; ~>20ms; 1~>C'] = 1,
        ['~>20ms; 5~>B; 2~>C'] = 10,
    }
}

-- testa ParOr que da clean em await vivo
Test { [[
input int A,B,C;
par/and do
    par/or do
        await A;
    with
        await B;
    end;
with
    await C;
end;
escape 100;
]],
    run = { ['1~>A;1~>C']=100 }
}

Test { [[
input int A;
var int b;
if true then
    await A;
    b = 1;
else
    if true then
        await A;
        b = 1;
    else
        await A;
        b = 0;
    end;
end;
escape b;
]],
    run = {
        ['0~>A ; 0~>A'] = 1,
    },
}

Test { [[
input int A;
var int b;
if true then
    await A;
    b = 1;
else
    if true then
        await A;
        b = 1;
    else
        await A;
    end;
end;
escape b;
]],
    ref = 'line 9 : missing initialization for variable "b" in the other branch of the `if-then-else´ (tests.lua:7)'
}

-->>> LOOP

Test { [[
loop i in 10 do
end
escape 1;
]],
    env = 'TODO: not a pool',
}

Test { [[
var int ret = 0;
loop i in [0 |> 256-1[ do
    ret = ret + 1;
end
escape ret;
]],
    run = 255,
}

Test { [[
var int ret = 0;
loop i in [1 |> 4] do
    ret = ret + i;
end
escape ret;
]],
    run = 10,
}

Test { [[
var int ret = 0;
loop i in [1|>4], 2 do
    ret = ret + i;
end
escape ret;
]],
    run = 4,
}

Test { [[
var int ret = 0;
loop i in [1|>4], -2 do
    ret = ret + i;
end
escape ret;
]],
    run = 'TODO: must be positive',
}

Test { [[
var int ret = 1;
loop i in [4|>1] do
    ret = ret + i;
end
escape ret;
]],
    run = 1,
}

Test { [[
var int ret = 1;
loop i in ]-3|>3[ do
    ret = ret + i;
end
escape ret;
]],
    run = 5,
}

Test { [[
var int sum = 0;
loop i in [_|>0] do
    if i == 10 then
        break;
    end
    sum = sum + 1;
end
escape sum;
]],
    parser = 'line 2 : after `_´ : expected `<|´',
}
Test { [[
var int sum = 0;
loop i in [0<|_] do
    if i == 10 then
        break;
    end
    sum = sum + 1;
end
escape sum;
]],
    parser = 'line 2 : after `<|´ : expected expression',
}

Test { [[
var int sum = 0;
loop i in [0|>_] do
    if i == 10 then
        break;
    end
    sum = sum + 1;
end
escape sum;
]],
    loop = true,
    tight = 'tight loop',
    run = 10,
}

Test { [[
var int ret = 0;
loop i in [1 <| 4] do
    ret = ret + i;
end
escape ret;
]],
    run = 10,
}

Test { [[
var int ret = 0;
loop i in [1<|4], 1 do
    ret = ret + i;
end
escape ret;
]],
    run = 10,
}

Test { [[
var int ret = 0;
loop i in [1<|4], 2 do
    ret = ret + i;
end
escape ret;
]],
    run = 6,
}
Test { [[
var int ret = 1;
loop i in [4<|1] do
    ret = ret + i;
end
escape ret;
]],
    run = 1,
}

Test { [[
var int ret = 1;
loop i in ]-3 <| 3] do
    ret = ret + i;
end
escape ret;
]],
    run = 6,
}

Test { [[
var int ret = 1;
loop i in [-10 <| -3[ do
    ret = ret + i;
end
escape ret;
]],
    run = 13,
}

Test { [[
var int sum = 0;
loop i in [_<|0] do
    if i == -10 then
        break;
    end
    sum = sum + 1;
end
escape sum;
]],
    loop = true,
    tight = 'tight loop',
    run = 10,
}

Test { [[
loop i in [-1 <| 0] do
end
escape 1;
]],
    --tight = 'line 1 : tight loop',
    run = 1,
}

Test { [[
var int n = 10;
var int sum = 0;
loop i in [0|>n[ do
    sum = sum + 1;
end
escape n;
]],
    loop = true,
    tight = 'tight loop',
    run = 10,
}

Test { [[
var int sum = 0;
loop i do
    if i == 10 then
        break;
    end
    sum = sum + 1;
end
escape sum;
]],
    loop = true,
    tight = 'tight loop',
    run = 10,
}

Test { [[
break;
]],
    props = 'line 1 : `break´ without loop',
}

Test { [[
input int A;
loop do
    do
        break;
    end;
end;
escape 1;
]],
    _ana = {
        unreachs = 1,    -- re-loop
    },
    run = 1,
}
Test { [[
input int A;
loop do
    do
        escape 1;
    end;
end;
escape 0;
]],
    _ana = {
        unreachs = 2,
    },
    run = 1,
}

Test { [[
input int A;
loop do
    loop do
        escape 1;
    end;
end;
escape 0;
]],
    _ana = {
        unreachs = 3,
    },
    run = 1,
}

Test { [[
input int A;
loop do
    loop do
        break;
    end;
end;
escape 0;
]],
    _ana = {
        isForever = true,
        unreachs = 2,
    },
    --tight = 'tight loop',
}

Test { [[
loop do
    par do
        await FOREVER;
    with
        break;
    end;
end;
escape 1;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = 1,
}

Test { [[
input int A,B;
loop do
    par do
        await A;
        await FOREVER;
    with
        await B;
        break;
    end;
end;
escape 1;
]],
    _ana = {
        unreachs = 1,
    },
    run = { ['1~>A;2~>B']=1, }
}

Test { [[
loop do
    par do
        await FOREVER;
    with
        escape 1;
    end;
end;        // unreachs
escape 1;   // unreachs
]],
    _ana = {
        unreachs = 2,
        abrt = 1,
    },
    run = 1,
}

Test { [[
input void A,B;
loop do
    par do
        await A;
        await FOREVER;
    with
        await B;
        escape 1;
    end;
end;        // unreachs
escape 1;   // unreachs
]],
    _ana = {
        unreachs = 2,
    },
    run = { ['~>A;~>B']=1, }
}

Test { [[
loop do
    async do
        break;
    end;
end;
escape 1;
]],
    props = '`break´ without loop',
}

Test { [[
input int A;
var int v = 0;
var int a = 0;
loop do
    a = 0;
    v = await A;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
var int a;
loop do a=1; end;
escape a;
]],
    ref = 'line 1 : uninitialized variable "a" crossing compound statement (tests.lua:2)',
}

Test { [[
var int a=0;
loop do a=1; end;
escape a;
]],
    --_ana = {
        --isForever = true,
        --unreachs = 1,
    --},
    tight = 'tight loop',
}

Test { [[break; escape 1;]],
    parser="line 1 : after `;´ : expected end of file"
}
Test { [[break; break;]],
    parser="line 1 : after `;´ : expected end of file"
}
Test { [[loop do break; end; escape 1;]],
    _ana = {
        unreachs=1,
    },
    run=1
}
Test { [[
var int ret=0;
loop do
    ret = 1;
    break;
end;
escape ret;
]],
    _ana = {
        unreachs = 1,
    },
    run = 1,
}

Test { [[
var int a=0;
loop do
    loop do
        a = 1;
    end;
end;
]],
    _ana = {
        isForever = true,
        unreachs = 1,
    },
    --tight = 'tight loop'
}

Test { [[
loop do
    loop do
        break;
    end;
end;
]],
    tight = 'tight loop',
    --_ana = {
        --isForever = true,
        --unreachs = 1,
    --},
}

Test { [[
loop do
    loop do
        await FOREVER;
    end;
end;
]],
    _ana = {
        unreachs = 2,
        isForever = true,
    },
}

Test { [[
loop i in [0 |> -1] do
end
escape 1;
]],
    --loop = true,
    wrn = true,
    run = 1,
    -- TODO: with sval -1 would be constant
}
Test { [[
loop i in [0 |> 0] do
end
escape 1;
]],
    run = 1,
}

Test { [[
input void A;
loop do
    loop do
        await A;
    end
end
]],
    _ana = { isForever=true },
}

Test { [[
input void A;
loop do
    loop i in [0|>1[ do
        await A;
    end
end
]],
    _ana = { isForever=true },
}

Test { [[
input void OS_START;
var int v = 1;
loop do
    loop i in [0|>v[ do
        await OS_START;
        escape 2;
    end
end
escape 1;
]],
    ana = 'line 4 : `loop´ iteration is not reachable',
    --ana = 'line 4 : statement is not reachable',    -- TODO: should be line 7
    run = 2,
}

Test { [[
input void OS_START;
var int v = 1;
loop do
    loop i in [0|>v[ do
        await OS_START;
        escape 2;
    end
end
escape 1;
]],
    wrn = true,
    run = 2,
}

Test { [[
native _assert;
input void OS_START;
event void a;
loop do
    if true then
        par do
            await a;
            break;
        with
            await OS_START;
            emit a;
            _assert(0);
        end
    else
        await OS_START;
    end
end
await 1s;
escape 1;
]],
    run = {['~>1s']=1},
}
-- LOOP / BOUNDED

Test { [[
native _V;
native do
    int V;
end
loop/_V do
end
escape 1;
]],
    tight = 'line 4 : `loop´ bound must be constant',
}
Test { [[
native/const _V;
native do
    int V;
end
loop/_V do
end
escape 1;
]],
    gcc = '5:5: error: variable-sized object may not be initialized',
}
Test { [[
loop/10 do
end
escape 1;
]],
    asr = 'runtime error: loop overflow',
    --run = 1,
}

Test { [[
loop/10000000 i in [0|>0[ do
end
escape 1;
]],
    run = 1,
}
Test { [[
var int ret = 0;
loop/3 do
    ret = ret + 1;
end
escape ret;
]],
    asr = 'runtime error: loop overflow',
}

Test { [[
loop/10 i in [0|>10[ do
end
escape 1;
]],
    run = 1,
}
Test { [[
var int a = 0;
loop/a i do
end
escape 1;
]],
    tight = '`loop´ bound must be constant',
}
Test { [[
loop/10 i do
end
escape 1;
]],
    asr = true,
}
Test { [[
native/const _A;
native do
    ##define A 10
end
#define A 10

var int ret = 0;
var int lim = 10 + 10 + _A + A;
loop/(10+10+_A+A) i in [0|>lim[ do
    ret = ret + 1;
end
escape ret;
]],
    run = 40;
}

Test { [[
native _printf;
var int k = 5;
loop/1 i in [0|>k[ do
    var int x = i + 2;
    _printf("%d\n", x);
end
escape 1;
]],
    run = '2] runtime error: loop overflow',
}

Test { [[
native _printf;
var int k = 5;
loop/10 i in [0|>k[ do
    var int x = i + 2;
    _printf("%d\n", x);
end
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
event void e;
every OS_START do
    loop i in [0|>10[ do
        emit e;
    end
    do break; end
end
escape 10;
]],
    props = 'line 7 : not permitted inside `every´',
}

Test { [[
input void OS_START;
event void e;
loop do
    await OS_START;
    loop i in [0|>10[ do
        emit e;
    end
    do break; end
end
escape 10;
]],
    ana = 'line 3 : `loop´ iteration is not reachable',
    run = 10,
}

Test { [[
input void OS_START;
event void a, b, c, d;
native _assert;
var int v=0;
par do
    loop do
        await OS_START;
        v = 0;
        emit a;
        v = 1;
        escape v;
    end
with
    loop do
        await a;
        v = 2;
    end
end
]],
    wrn = true,
    run = 1,
}

Test { [[
input void OS_START;
event void a, b, c, d;
native _assert;
var int v=0;
par do
    loop do
        await OS_START;
        v = 0;
        emit a;
        v = 1;
        escape v;
    end
with
    loop do
        await a;
        v = 2;
    end
end
]],
    safety = 2,
    wrn = true,
    run = 1,
    _ana = {
        acc = 3,
    },
}

Test { [[
input int E;
var int x=0;
loop do
    var int tmp = await E;
    if tmp == 0 then
        break;              // non-ceu code might have cleared x on stack
    end
    x = tmp;
end
escape x;
]],
    run = { ['1~>E; 2~>E;0~>E']=2 }
}

Test { [[
native do ##include <assert.h> end
input void OS_START;
event void a, b, c, d;
native _assert;
var int v=0;
par do
    loop do
        await OS_START;
        emit a;         // killed
        _assert(0);
    end
with
    loop do
        await a;
        escape 1;       // kills emit a
    end                 // unreach
end
]],
    _ana = {
        unreachs = 1,
        excpt = 1,
    },
    run = 1,
}

Test { [[
input void B;
var int a = 0;
loop do
    par/or do       // 4
        await 2s;
    with
        a = a + 1;          // 7
        await B;
        break;
    with
        await 1s;   // 11
        loop do
            a = a * 2;      // 13
            await 1s;   // 14
        end
    end
end
escape a;
]],
    _ana = {
        abrt = 2,
    },
    run = { ['~>5s; ~>B']=14 },
}

Test { [[
input void B;
var int a = 0;
loop do
    par/or do       // 4
        await 2s;
    with
        a = a + 1;          // 7
        await B;
        break;
    with
        await 1s;   // 11
        loop do
            a = a * 2;      // 13
            await 1s;   // 14
        end
    end
end
escape a;
]],
    _ana = {
        abrt = 2,
        acc  = 3,
    },
    run = { ['~>5s; ~>B']=14 },
    safety = 2,
}

Test { [[
var int a=0;
loop do
    par/or do
        await 2s;
    with
        a = 1;
        await FOREVER;
    with
        await 1s;
        loop do
            a = 2;
            await 1s;
        end
    end
end
]],
    _ana = {
        isForever = true,
        --acc = 1,
        abrt = 2,
    },
}

Test { [[
input int A;
loop do
    await A;
    await 2s;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
input int A;
par do
    loop do
        await A;
        await 2s;
    end;
with
    loop do
        await 2s ;
    end;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
var int i;
loop i do
end
escape 0;
]],
    locs = 'line 2 : implicit declaration of "i" hides previous declaration (tests.lua : line 1)',
}

-- EVERY

Test { [[
par/or do
    nothing;
with
    every (2)s do
        nothing;
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
input void A;
var int ret = 0;
every A do
    ret = ret + 1;
    if ret == 3 then
        escape ret;
    end
end
]],
    props = 'line 6 : not permitted inside `every´',
}

Test { [[
input int E;
var int x;
every x in E do
end
escape 1;
]],
    locs = 'line 3 : implicit declaration of "x" hides previous declaration',
}

Test { [[
input void A;
var int ret = 0;
loop do
    await A;
    ret = ret + 1;
    if ret == 3 then
        escape ret;
    end
end
]],
    run = { ['~>A;~>A;~>A']=3 }
}

Test { [[
var int ret = 0;
every 1s do
    await 1s;
    ret = ret + 1;
    if ret == 10 then
        escape ret;
    end
end
]],
    props = 'line 3 : `every´ cannot contain `await´',
}

Test { [[
var int ret = 0;
loop do
    await 1s;
    ret = ret + 1;
    if ret == 10 then
        escape ret;
    end
end
]],
    run = { ['~>10s']=10 }
}

Test { [[
var int ret = 0;
var int dt;
loop do
    var int dt = await 1s;
    ret = ret + dt;
    if ret == 10000000 then
        escape ret;
    end
end
]],
    locs = 'line 4 : declaration of "dt" hides previous declaration',
}

Test { [[
var int ret = 0;
loop do
    var int dt = await 1s;
    ret = ret + dt;
    if ret == 10000000 then
        escape ret;
    end
end
]],
    run = { ['~>5s']=10000000 }
}

Test { [[
event void inc;
loop do
    await inc;
    nothing;
end
every inc do
    nothing;
end
]],
    _ana = { isForever=true },
}

Test { [[
input (int,int) A;
par do
    var int a, b;
    (a,b) = await A;
    if a and b then end
with
    await A;
    escape 1;
with
    async do
        emit A => (1,1);
    end
end
]],
    run = 1;
}

Test { [[
input (int,int) A;
par do
    var int a, b;
    (a,b) = await A;
    if a and b then end
with
    escape 1;
end
]],
    run = 1;
}

Test { [[
input (int,int) A;
async do
    emit A => (1,3);
end
escape 1;
]],
    run = 1;
}

Test { [[
input (int,int) A;
par do
    loop do
        var int a, b;
        (a,b) = await A;
        escape a+b;
    end
with
    async do
        emit A => (1,3);
    end
end
]],
    wrn = true,
    run = 4;
}
Test { [[
input (int,int) A;
par do
    loop do
        var int a, b;
        (a,b) = await A;
        escape a+b;
    end
with
    async do
        emit A => (1,3);
    end
end
]],
    ana = 'line 3 : `loop´ iteration is not reachable',
    --run = 4;
}

Test { [[
input (int,int) A;
par do
    var int a, b;
    every (a,b) in A do
        escape a+b;
    end
with
    async do
        emit A => (1,3);
    end
end
]],
    locs = 'line 4 : implicit declaration of "a" hides previous declaration',
}
Test { [[
input (int,int) A;
par do
    every (a,b) in A do
        escape a+b;
    end
with
    async do
        emit A => (1,3);
    end
end
]],
    props = 'line 4 : not permitted inside `every´',
}
Test { [[
input (int,int) A;
par do
    loop do
        var int a,b;
        (a,b) = await A;
        escape a+b;
    end
with
    async do
        emit A => (1,3);
    end
end
]],
    ana = 'line 3 : `loop´ iteration is not reachable',
}
Test { [[
input (int,int) A;
par do
    var int a, b;
    loop do
        (a,b) = await A;
        escape a+b;
    end
with
    async do
        emit A => (1,3);
    end
end
]],
    ref = 'line 3 : uninitialized variable "a" crossing compound statement (tests.lua:4)',
}

Test { [[
input (int,int) A;
par do
    loop do
        var int a, b;
        (a,b) = await A;
        escape a+b;
    end
with
    async do
        emit A => (1,3);
    end
end
]],
    wrn = true,
    run = 4;
}

Test { [[
input void A,C;
var int ret = 0;
par/or do
    every A do
        ret = ret + 1;
    end
with
    await C;
end
escape ret;
]],
    run = { ['~>A;~>A;~>A;~>C;~>A']=3 },
}

Test { [[
every 1s do
    break;
end
]],
    props = 'line 2 : not permitted inside `every´',
}

Test { [[
every 1s do
    escape 1;
end
]],
    props = 'line 2 : not permitted inside `every´',
}

Test { [[
every 1s do
    loop do
        if true then
            break;
        end
    end
end
]],
    tight = 'line 2 : tight loop',
}

Test { [[
par do
    every 1s do
        var int ok = do/_
            escape 1;
        end;
        if ok then end;
    end
with
    await 2s;
    escape 10;
end
]],
    run = { ['~>10s'] = 10 },
}

-->>> CONTINUE

Test { [[
var int ret = 1;
loop i in [0|>10[ do
    if true then
        continue;
    end
    ret = ret + 1;
    if false then
        continue;
    end
end
escape ret;
]],
    run = 1,
}

Test { [[
loop do
    if false then
        continue;
    else
        nothing;
    end
end
]],
    adj = 'line 3 : invalid `continue´',
}

Test { [[
loop do
    do continue; end
end
]],
    adj = 'line 2 : invalid `continue´',
}

Test { [[
loop do
    do
        if false then
            continue;
        end
    end
end
]],
    adj = 'line 4 : invalid `continue´',
}

Test { [[
loop do
    if false then
        continue;
    end
    await 1s;
end
]],
    --tight = 'tight loop',
    _ana = {
        isForever = true,
        --unreachs = 1,
    },
}

Test { [[
var int ret = 0;
loop i in [0|>10[ do
    if i%2 == 0 then
        ret = ret + 1;
        await 1s;
        continue;
    end
    await 1s;
end
escape ret;
]],
    run = { ['~>10s']=5 }
}

Test { [[
every 1s do
    if true then
        continue;
    end
end
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
par/or do
    loop do
        if false then
            continue;
        end
        var int dim = 0;
        var int x = dim;
        if x then end
        do break; end
    end
with
end
escape 1;
]],
    wrn = true,
    loop = true,
    run = 1,
}

Test { [[
var int x = 0;
loop i in [0|>10[ do
    x = x + 1;
    par/and do
        await FOREVER;
    with
        continue/i;
    end
end
escape x;
]],
    run = 10,
}

Test { [[
var int x = 0;
loop i in [0|>10[ do
    loop j in [0|>10[ do
        x = x + 1;
        par/and do
            await FOREVER;
        with
            continue/i;
        end
    end
end
escape x;
]],
    run = 10,
}

Test { [[
var int x = 0;
loop i in [0|>10[ do
    loop j in [0|>10[ do
        x = x + 1;
        par/and do
            await FOREVER;
        with
            continue/j;
        end
    end
end
escape x;
]],
    run = 100,
}

Test { [[
var int x = 0;
loop i in [0 |> 10[ do
    x = x + 1;
    par/and do
        await FOREVER;
    with
        break/i;
    end
end
escape x;
]],
    run = 1,
}

Test { [[
var int x = 0;
loop i in [0|>10[ do
    loop j in [0|>10[ do
        x = x + 1;
        par/and do
            await FOREVER;
        with
            break/i;
        end
    end
end
escape x;
]],
    run = 1,
}

Test { [[
var int x = 0;
loop i in [0|>10[ do
    loop j in [0|>10[ do
        x = x + 1;
        par/and do
            await FOREVER;
        with
            break/j;
        end
    end
end
escape x;
]],
    run = 10,
}

--<<< CONTINUE

-- EX.05
Test { [[
input int A;
loop do
    await A;
end;
]],
    _ana = {
        isForever = true,
    },
}
Test{ [[
input int E;
var int a=0;
loop do
    a = await E;
end;
]],
    _ana = {
        isForever = true,
    },
}
Test{ [[
input int E;
loop do
    var int v = await E;
    if v then
    else
    end;
end;
]],
    _ana = {
        isForever = true,
    },
}
Test { [[
var int a=0;
loop do
    if false then
        a = 0;
    else
        a = 1;
    end;
end;
escape a;
]],
    tight = 'tight loop',
}
Test { [[
var int a=0;
loop do
    if false then
        a = 0;
    else
        a = 1;
    end;
end;
escape a;
]],
    --tight = 'tight loop',
    _ana = {
        isForever = true,
        unreachs = 1,
    },
}
Test { [[
loop do
    if false then
        break;
    end;
end;
escape 0;
]],
    tight = 'line 1 : tight loop',
}

Test { [[
par/or do
    loop do
    end;
with
    loop do
    end;
end;
escape 0;
]],
    loop='tight loop',
    _ana = {
        isForever = true,
        unreachs = 2,
    },
}

Test { [[
par/and do
    loop do
    end;
with
    loop do
    end;
end;
escape 0;
]],
    loop='tight loop',
    _ana = {
        isForever = true,
        unreachs = 2,
    },
}

Test { [[
event int a;
par/and do
    await a;
with
    loop do end;
end;
escape 0;
]],
    loop='tight loop',
    _ana = {
        isForever = true,
        unreachs = 2,
    },
}

Test { [[
input int A;
loop do
    par/or do
        await A;
    with
    end;
end;
escape 0;
]],
    loop='tight loop',
    _ana = {
        isForever = true,
        unreachs = 1,
        abrt = 1,
    },
}
Test { [[
input int A;
var int a;
a = 0;
loop do
    par/or do
    with
        await A;
    end;
end;
escape 0;
]],
    loop='tight loop',
    _ana = {
        abrt = 1,
        isForever = true,
        unreachs = 1,
    },
}

Test { [[
input void OS_START;
event void a,b;
par/and do
    await a;
with
    await OS_START;
    emit b;
    emit a;
end
escape 5;
]],
    run = 5,
}

Test { [[
input int A;
if false then
    loop do await A; end;
else
    loop do await A; end;
end;
escape 0;   // TODO
]],
    _ana = {
        unreachs = 1,
        isForever = true,
    },
}
Test { [[
input int A;
if false then
    loop do await A; end;
else
    loop do await A; end;
end;
]],
    _ana = {
        isForever = true,
    },
}
Test { [[
input int A;
loop do
    if false then
        await A;
    else
        break;
    end;
end;
escape 1;
]],
    run = 1,
}
Test { [[
input int A;
loop do
    if false then
        await A;
        await A;
    else
        break;
    end;
end;
escape 1;
]],
    run = 1,
}
Test { [[
input int C, A;
loop do
    var int v = await C;
    if v then
        await A;
    else
        break;
    end;
end;
escape 1;
]],
    run = {
        ['0~>C'] = 1,
        ['1~>C;0~>A;0~>C'] = 1,
    }
}

Test { [[
input int A;
var int a=0;
par/or do           // 3
    loop do
        a = 1;      // 5
        await A;    // 6
    end;
with
    await A;        // 9
    await A;
    a = 1;          // 11
end;
escape a;
]],
    _ana = {
        abrt = 1,
        acc = 1,
    },
}

Test { [[
input int A;
var int a=0;
par do
    loop do
        par/or do
            a = 1;      // 6
            await A;    // 7
        with
            await A;    // 9
            a = 2;      // 10
        end;
    end
with
    loop do
        await A;
        a = 3;          // 16
    end
end
]],
    _ana = {
        isForever = true,
        acc = 2,        -- 6/16  10/16
        abrt = 3,
    },
}

Test { [[
input int A;
var int a=0;
par do
    loop do
        par/or do
            a = 1;      // 6
            await A;    // 7
        with
            await A;    // 9
            a = 2;      // 10
        end;
    end
with
    loop do
        await A;
        a = 3;          // 16
    end
end
]],
    _ana = {
        isForever = true,
        acc = 3,        -- 6/16  10/16  6/10
        abrt = 3,
    },
    safety = 2,
}

-- FOR

Test { [[
input int A;
var int sum = 0;
par/or do
    loop i in [0|>1+1[ do
        await A;
    end
    sum = 0;
with
    sum = 1;
end
escape sum;
]],
    todo = 'for',
    _ana = {
        acc = 1,
        unreachs = 1,
    },
    run = 1,
}

Test { [[
input int A;
var int sum = 0;
par/or do
    loop i in [0|>1[ do    // 4
        await A;
    end
    sum = 0;
with
    sum = 1;        // 9
end
escape sum;
]],
    _ana = {
        abrt = 1,
        --unreachs = 2,
    },
    run = 1,
}

Test { [[
input void A;
var int sum = 0;
var int ret = 0;
par/or do
    loop i in [0|>2[ do
        await A;
        ret = ret + 1;
    end
    sum = 0;    // 9
with
    await A;
    await A;
    sum = 1;    // 13
end
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 5,    -- TODO: not checked
    },
    run = { ['~>A; ~>A; ~>A']=2 },
}

Test { [[
input void A;
var int sum = 0;
var int ret = 0;
par/or do
    loop i in [0|>3[ do
        await A;
        ret = ret + 1;
    end
    sum = 0;
with
    await A;
    await A;
    sum = 1;
end
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 5,    -- TODO: not checked
    },
    run = { ['~>A;~>A'] = 2 },
    --todo = 'nd excpt',
}

Test { [[
input int A;
var int sum = 0;
par/or do
    loop i in [0|>1[ do    // 4
        await A;
        async do
            var int a = 1;
            if a then end
        end
    end
    sum = 0;
with
    sum = 1;        // 12
end
escape sum;
]],
    _ana = {
        abrt = 1,
        --unreachs = 3,
    },
    run = 1,
}

Test { [[
input int A;
var int sum = 0;
par/or do
    sum = 5;            // 4
    loop i in [0|>10[ do       // 5
        await A;
        async do
            var int a = 1;
            if a then end
        end
    end
    sum = 0;            // 11
with
    loop i in [0 |> 2[ do        // 13
        async do
            var int a = 1;
            if a then end
        end
        sum = sum + 1;  // 17
    end
end
escape sum;
]],
    run = 7,
}

Test { [[
input int A;
var int sum = 0;
par/or do
    sum = 5;            // 4
    loop i in [0 |> 10[ do       // 5
        await A;
        async do
            var int a = 1;
            if a then end
        end
    end
    sum = 0;            // 11
with
    loop i in [0 |> 2[ do        // 13
        async do
            var int a = 1;
            if a then end
        end
        sum = sum + 1;  // 17
    end
end
escape sum;
]],
    run = 7,
    safety = 2,
    _ana = {
        acc = 4,
    },
}

Test { [[
var int sum = 0;
loop i in [0 |> 100[ do
    sum = sum + (i+1);
end
escape sum;
]],
    --loop = true,
    run = 5050,
}
Test { [[
var int sum = 0;
for i=1, 100 do
    i = 1;
    sum = sum + i;
end
escape sum;
]],
    --loop = true,
    todo = 'should raise an error',
    run = 5050,
}
Test { [[
var int sum = 5050;
loop i in [0 |> 100[ do
    sum = sum - (i+1);
end
escape sum;
]],
    --loop = true,
    run = 0,
}
Test { [[
var int sum = 5050;
var int v = 0;
loop i in [0 |> 100[ do
    v = i;
    if sum == 100 then
        break;
    end
    sum = sum - (i+1);
end
escape v;
]],
    --loop = true,
    run = 99,
}
Test { [[
input void A;
var int sum = 0;
var int v = 0;
loop i in [0 |> 101[ do
    v = i;
    if sum == 6 then
        break;
    end
    sum = sum + i;
    await A;
end
escape v;
]],
    run = {['~>A;~>A;~>A;~>A;~>A;~>A;~>A;~>A;~>A;~>A;']=4},
}
Test { [[
var int sum = 4;
loop i in [0 |> 0[ do
    sum = sum - i;
end
escape sum;
]],
    --loop = true,
    --adj = 'line 2 : constant should not be `0´',
    run = 4,
}
Test { [[
input void A;
var int sum = 0;
loop i in [0 |> 10[ do
    await A;
    sum = sum + 1;
end
escape sum;
]],
    run = {['~>A;~>B;~>A;~>A;~>A;~>A;~>A;~>A;~>A;~>A;~>A;']=10},
}

--<<< LOOP

Test { [[
input int A,B;
var int ret=0;
par/and do
    ret = await A;
with
    ret = await B;
end;
escape ret;
]],
    run = { ['1~>A;2~>B'] = 2 }
}

Test { [[
input int A,B,Z,X,C;
var int ret=0;
par/and do
    ret = await A;
with
    ret = await B;
end;
escape ret;
]],
    run = { ['1~>A;2~>B'] = 2 },
    safety = 2,
    _ana = {
        acc = 1,
    },
}

Test { [[
input int A,B,Z,X,C;
var int ret=0;
par/or do
    par/and do
        ret = await A;
    with
        ret = await B;
    end;
    par/or do
        par/or do               // 10
            ret = await B;
        with
            ret = await Z;      // 13 (false w/ 10)
        end;
    with
        ret = await X;          // 16 (false w/10,9)
    end;
with
    ret = await C;
end;
escape ret;
]],
    run = { ['1~>C'] = 1 }
}

Test { [[
input int A,B,Z,X,C;
var int ret=0;
par/or do
    par/and do
        ret = await A;
    with
        ret = await B;
    end;
    par/or do
        par/or do               // 10
            ret = await B;
        with
            ret = await Z;      // 13 (false w/ 10)
        end;
    with
        ret = await X;          // 16 (false w/10,9)
    end;
with
    ret = await C;
end;
escape ret;
]],
    run = { ['1~>C'] = 1 },
    safety = 2,
    _ana = {
        acc = 9,
    },
}

Test { [[
input int A;
var int a = 0;
loop do
    await A;
    a = a + 1;
    break;
end;
await A;
await A;
escape a;
]],
    _ana = {
        unreachs = 1,
    },
    run = { ['0~>A;0~>A;0~>A'] = 1 }
}

Test { [[
input int C;
var int a = 0;
par do
    a = a + 1;
    await FOREVER;
with
    await C;
    escape a;
end;
]],
    _ana = {
        isForever = false,
    },
    run = { ['~>1min; ~>1min ; 0~>C'] = 1 },
}

Test { [[
input int C;
var int a = 0;
par do
    a = a + 1;
    await FOREVER;
with
    await C;
    escape a;
end;
]],
    safety = 2,
    _ana = {
        isForever = false,
        acc = 1,
    },
    run = { ['~>1min; ~>1min ; 0~>C'] = 1 },
}

Test { [[
input int A;
var int a = await A;
await A;
escape a;
]],
    run = {['10~>A;20~>A']=10},
}

Test { [[
input int A;
var int a = await A;
var int b = await A;
escape a + b;
]],
    run = { ['10~>A;20~>A']=30, ['3~>A;0~>A;0~>A']=3 }
}

-- A changes twice, but first value must be used
Test { [[
input int A,C;
var int a=0,f=0;
par/and do
    a = await A;
with
    f = await C;
end;
escape a+f;
]],
    run = { ['1~>A;5~>A;1~>C'] = 2 },
}

Test { [[
input int A,C;
var int a=0,f=0;
par/or do
    par do
        a = await A;
    with
        await FOREVER;
    end
with
    f = await C;
end;
escape a+f;
]],
    run = { ['1~>A;5~>A;1~>C'] = 2 },
}

-->>> INTERNAL EVENTS

Test { [[
input void OS_START;
event int a;
var int ret = 0;
par/or do
    await OS_START;
    emit a => 1;
with
    ret = await a;
end
escape ret;
]],
    run = 1,
    _ana = {
        excpt = 1,
    },
}

Test { [[
input void OS_START;
var int ret=0;
event void a,b;
par/and do
    await OS_START;
    emit a;
with
    await OS_START;
    emit b;
end
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
var int ret=0;
event void a,b;
par/and do
    await OS_START;
    emit a;
with
    await OS_START;
    emit b;
with
    await a;
    ret = 1;    // 12: nd
with
    await b;
    ret = 2;    // 15: nd
end
escape ret;
]],
    _ana = {
        acc = 1,
    },
    run = 2,
}

Test { [[
input void OS_START;
var int ret=0;
event void a,b;
par/and do
    await OS_START;
    emit a;         // 6
with
    par/or do
        await OS_START;
    with
        await 1s;
    end
    emit b;         // 13
with
    await a;        // 15
    ret = 1;        // acc
with
    par/or do
        await b;    // 19
    with
        await 1s;   // 21
    end
    ret = 2;        // acc
end
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = 2,
}

Test { [[
input void OS_START;
var int ret=0;
event void a,b,c,d;
par/and do
    await OS_START;
    emit a;
with
    await OS_START;
    emit b;
with
    await a;
    emit c;
with
    await b;
    emit d;
with
    await c;
    ret = 1;    // 18: acc
with
    await d;
    ret = 2;    // 21: acc
end
escape ret;
]],
    _ana = {
        acc = 1,
    },
    run = 2,
}

Test { [[
event int c;
emit c => 10;
await c;
escape 0;
]],
    _ana = {
        --unreachs = 1,
        --isForever = true,
    },
    --trig_wo = 1,
}

-- EX.06: 2 triggers
Test { [[
event int c;
emit c => 10;
emit c => 10;
escape c;
]],
    env = 'line 4 : types mismatch (`int´ <= `void´)',
    --trig_wo = 2,
}

Test { [[
event int c;
emit c => 10;
emit c => 10;
escape 10;
]],
    run = 10,
    --trig_wo = 2,
}

Test { [[
event int b;
var   int a;
a = 1;
emit b => a;
escape a;
]],
    run = 1,
    --trig_wo = 1,
}

Test { [[
input void OS_START;
event float x;
var float ret = 0;
par/and do
    ret = await x;
with
    await OS_START;
    emit x => 1.1;
end
escape ret>1.0 and ret<1.2;
]],
    run = 1,
}

Test { [[
input float X;
var float ret=0;
par/and do
    ret = await X;
with
    async do
        emit X => 1.1;
    end
end
escape ret>1.0 and ret<1.2;
]],
    run = 1,
}

-- RESEARCH-5
Test { [[
input void OS_START;
event int e;
par do
    await OS_START;
    emit e => 1;
    escape -1;
with
    await e;
    emit e => 2;
    escape -2;
with
    var int v = await e;
    escape v;   // 1
end
]],
    _ana = {acc=true},
    run = 2,
    --run = -2,
}
Test { [[
input void OS_START;
event int e;
par do
    await OS_START;
    emit e => 1;
    escape -1;
with
    await e;
    emit e => 2;
    await FOREVER;
with
    var int v = await e;
    escape v;   // 1
end
]],
    _ana = {acc=true},
    run = 2,
    --run = 1,
}

-- the inner "emit e" is aborted and the outer "emit e"
-- awakes the last "await e"
Test { [[
input void OS_START;

event int e;

var int ret = 0;

par/or do
    await OS_START;
    emit e => 2;
    escape -1;
with
    par/or do
        await e;
        emit e => 3;
        escape -2;
    with
        var int v = await e;
        ret = ret + v;          // 0+3
    end
    await FOREVER;
with
    var int v = await e;
    ret = ret * v;              // 3*[2,3]
end

escape ret;
]],
    --_ana = {acc=3},
    _ana = {acc=true},
    run = 6,
    --run = 9,
    --run = -2,
}

Test { [[
input void OS_START;

event int e;

var int ret = 0;

par/or do
    await OS_START;
    emit e => 2;
    escape -1;
with
    par/or do
        await e;
        emit e => 3;
        await FOREVER;
    with
        var int v = await e;
        ret = ret + v;          // 0+3
    end
    await FOREVER;
with
    var int v = await e;
    ret = ret * v;              // 3*[2,3]
end

escape ret;
]],
    --_ana = {acc=3},
    _ana = {acc=true},
    run = 6,
    --run = 9,
    --run = 4,
}

-- "emit e" on the stack has to die
-- RESEARCH-1:
Test { [[
input void OS_START;

event int&& e;
var int ret = 0;

par/or do
    do
        var int i = 10;
        par/or do
            await OS_START;
            emit e => &&i;           // stacked
        with
            var int&& pi = await e;
            ret = *pi;
        end                         // has to remove from stack
    end
    do
        var int i = 20;
        await 1s;
        i = i + 1;
    end
with
    var int&& i = await e;           // to avoid awaking here
    escape *i;
end
escape ret;
]],
    env = 'line 3 : invalid event type',
    --env = 'line 11 : wrong argument : cannot pass pointers',
    --run = { ['~>1s']=10 },
}

Test { [[
event void e;
input void OS_START;

par do
    par do
        par/or do
            await e;
        with
            await FOREVER;
        end
    with
        await OS_START;
        emit e;
        escape 2;   // should continue after the awake below
    end
with
    await e;
    escape 1;       // should escape before the one above
end
]],
    run = 1,
}

Test { [[
event void e;
loop i in [0 |> 1000[ do
    emit e;
end
escape 1;
]],
    run = 1, -- had stack overflow
}
Test { [[
event void e;
var int ret = 0;
par/or do
    every e do
        ret = ret + 1;
    end
with
    loop i in [0 |> 2[ do
        emit e;
    end
end
escape ret;
]],
    _ana = {acc=1},
    run = 2,
}

Test { [[
event void e;
var int ret = 0;
par/or do
    every e do
        ret = ret + 1;
    end
with
    loop i in [0 |> 1000[ do
        emit e;
    end
end
escape ret;
]],
    _ana = {acc=1},
    run = 1000, -- had stack overflow
}

Test { [[
input void OS_START;
event (int,int) e;
par do
    do
        par/or do
            await OS_START;
            emit e => (1,2);
        with
            await e;
        end
    end
    do
        emit e => (3,4);
    end
with
    var int a,b;
    (a,b) = await e;
    escape a+b;
end
]],
    run = 7,
    --run = 3,
}

-- different semantics w/ longjmp
Test { [[
input void OS_START;
event void e,f;
par do
    par/or do
        await OS_START;
        emit e;
    with
        await f;
    end
    await 1s;
    escape 1;
with
    await e;
    emit f;     // this continuation dies b/c the whole stack
    escape 2;   // for emit-e dies
end
]],
    run = {['~>1s']=2},
}

-- EMIT / SELF-ABORT
Test { [[
native _assert;
input void I;
event void e, f;
par do
    watching e do       // 5
        await I;        // 1
        emit f;         // 3, aborted on 5
        _assert(0);     // never executes
    end
    await I;
    escape 42;
with
    await f;            // 2
    emit e;             // 4, aborted on 5
    //_assert(0);         // never executes
    escape -42;
with
    async do
        emit I;
        emit I;
    end
end
]],
    run = -42,
}

--<<< INTERNAL EVENTS

-- ParOr

Test { [[
input void OS_START;
event int a;
var int aa = 3;
par do
    await OS_START;
    emit a => aa;      // 6
    escape aa;
with
    loop do
        var int v = await a;
        aa = v+1;
    end;
end;
]],
    awaits = 0,
    run = 4,
}

Test { [[
input void OS_START;
event int a;
var int aa = 3;
par do
    await OS_START;
    emit a => aa;      // 6
    escape aa;
with
    loop do
        var int v = await a;
        aa = v+1;
    end;
end;
]],
    awaits = 0,
    run = 4,
    safety = 2,
    _ana = {
        acc = 2,
    },
}

Test { [[
input void OS_START;
event int a;
var int aa = 3;
par do
    await OS_START;
    emit a => aa;
    escape aa;
with
    loop do
        var int v = await a;
        aa = v+1;
    end;
end;
]],
    run = 4,
}

Test { [[
var int ret = 0;
event int a;
var int aa = 3;
par/or do
    await a;
    ret = ret + 1;  // 6
with
    ret = 5;        // 8
end
emit a;
escape ret;
]],
    --env = 'line 10 : missing parameters on `emit´',
    env = 'line 10 : arity mismatch',
}

Test { [[
var int ret = 0;
event int a;
var int aa = 3;
par/or do
    await a;
    ret = ret + 1;  // 6
with
    ret = 5;        // 8
end
emit a => 1;
escape ret;
]],
    _ana = {
        abrt = 1,
        --unreachs = 1,
    },
    run = 5,
}

Test { [[
vector[2] int v;
v[0] = 1;
var int ret=0;
par/or do
    ret = v[0];
with
    ret = v[1];
end;
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
input int A;
var int a = 0;
par/or do
    if true then
        a = await A;
    end;
with
    a = await A;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = { ['10~>A']=10 },
}

Test { [[
input int A,B;
var int a = do/_ par do
        await A;
        if true then
            await B;
            // unreachable
        end;
        escape 0;               // 8
    with
        var int v = await A;
        escape v;               // 11
end
    end;
escape a;
]],
    _ana = {
        --unreachs = 1,
        acc = 1,
        abrt = 3,
    },
}

Test { [[
input int A;
var int a;
a = do/_ par do
        if true then
            var int v = await A;
            escape v;           // 6
        end;
        escape 0;
    with
        var int v = await A;
        escape v;               // 11
end
    end;
escape a;
]],
    run = {['6~>A']=6},
    _ana = {acc=true},
    --ref = 'line 6 : missing initialization for variable "a" in the other branch of the `if-then-else´ (tests.lua:4)',
}
Test { [[
input int A;
var int a;
a = do/_ par do
        if true then
            var int v = await A;
            escape v;           // 6
        else
            escape 0;
        end;
    with
        var int v = await A;
        escape v;               // 11
end
    end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 4,
    },
}

Test { [[
input int A;
var int a;
a = do/_ par do
    await A;                    // 4
    if true then
        var int v = await A;
        // unreachable
        escape v;               // 8
    else
        escape 0;                   // 10
    end;
with
    var int v = await A;
    escape v;                   // 13
end
end;
escape a;
]],
    _ana = {
        --unreachs = 1,
        acc  = 2,
        abrt  = 6,
    },
    run = { ['1~>A']=1 },
}

Test { [[
input void OS_START;
event void e;
var int v=0;
par/or do           // 4
    await OS_START;
    emit e;         // 6
    v = 1;
with
    await e;        // 9
    emit e;
    v = 2;
end
escape v;
]],
    _ana = {
        excpt = 1,
    },
    run = 2,
}

Test { [[
input void OS_START;
event void e;
var int v=0;
par/or do           // 4
    await OS_START;
    emit e;         // 6
    v = 1;
with
    await e;        // 9
    emit e;
    v = 2;
end
escape v;
]],
    _ana = {
        excpt = 1,
    },
    run = 2,
    safety = 2,
    _ana = {
        acc = 1,
    },
}

Test { [[
input int A,B;
var int a,v=0;
a = do/_ par do
    if true then
        v = await A;    // 5
        escape 0;           // 10
    else
        await B;
        escape v;
    end;
with
    var int v = await A;
    escape v;           // 13
end
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}

Test { [[
input int A,B;
var int a,v=0;
a = do/_ par do
    if true then
        v = await A;
        escape v;       // 6
    else
        await B;
        escape v;
    end;
    escape 0;
with
    var int v = await A;
    escape v;           // 14
end
end;
escape a;
]],
    _ana = {
        unreachs = 1,
        acc = 1,
        abrt = 3,
    },
    run = { ['1~>A']=1 },
}

Test { [[
input void OS_START;
event void c,d;
par do
    await OS_START;
    emit c;
    escape 10;       // 35
with
    loop do
        await c;
        emit d;
    end
end
]],
    _ana = {
        acc = true,
        unreachs = 1,
        abrt = 1,
    },
    run = 10,
}

Test { [[
native do
    ##include <assert.h>
end
input void OS_START;
event void a, b, c, d;
native _assert;
var int v=0;
par do
    loop do
        await OS_START;
        _assert(v==0);
        v = v + 1;
        emit a;
        _assert(v==6);
    end
with
    loop do
        await a;
        _assert(v==1);
        v = v + 1;
        emit b;
        _assert(v==4);
        v = v + 1;
    end
with
    loop do
        await a;        // 24
        _assert(v==5);
        v = v + 1;
    end
with
    loop do
        await b;
        _assert(v==2);
        v = v + 1;
        emit c;
        _assert(v==4);
        escape v;       // 35
    end                 // unreach
with
    loop do
        await c;
        _assert(v==3);
        emit d;
        _assert(v==3);
        v = v + 1;
    end
end
]],
    _ana = {
        acc = true,
        unreachs = 1,
        abrt = 1,
    },
    run = 4,
}

Test { [[
input int A;
var int a = 0;
par/or do
    if true then
        a = await A;
    end;
with
    if not true then
        a = await A;
    end;
end;
escape a;
]],
    _ana = {
        acc  = 1,
        abrt  = 3,
    },
    run = 0,
}

Test { [[
input int B;
event int a;
var int aa=0;
par do
    await B;
    escape 1;
with
    await B;
    par/or do
    with
    end;
    escape 2;
end;
]],
    _ana = {
        --unreachs = 1,
        abrt = 6,      -- TODO: not checked
        acc = 1,
    },
}
Test { [[
input int B;
event int a;
var int aa=0;
par do
    await B;
    escape 1;
with
    await B;
    par/or do
        escape 2;
    with
    end;
    escape 3;
end;
]],
    _ana = {
        --unreachs = 1,
        abrt = 8,      -- TODO: not checked
        acc = 2,
    },
}

Test { [[
event void a, b;
input void OS_START,A;
var int ret = 0 ;

par/or do
    loop do
        await a;
        ret = ret + 1;
    end
with
    await b;
    emit a;
    await FOREVER;
with
    await OS_START;
    emit a;
    await A;
    emit b;
end
escape ret;
]],
    run = { ['~>A']=2 },
}

-- the second E cannot awake
Test { [[
input void E;
event void e;
var int ret = 1;
par do
    await E;
    emit e;
    ret = ret * 2;
    escape ret;
with
    await e;
    ret = ret + 1;
    await E;
    ret = ret + 1;
    escape 10;
end
]],
    _ana = { acc=true },
    run = { ['~>E']=4 },
}

-- TODO: STACK
Test { [[
event void a, b;
input void OS_START;
var int ret = 0 ;

par/or do
    loop do
        await a;
        ret = ret + 1;
    end
with
    await b;
    emit a;
    await FOREVER;
with
    await OS_START;
    emit a;
    emit b;
end
escape ret;
]],
    _ana = { acc=1 },
    --run = 2,
    run = 1,
}

-- TODO: STACK
-- internal glb awaits
Test { [[
input void OS_START;
event void a;
native _ret_val, _ret_end;
_ret_val = 0;
par do
    loop do
        par/or do
            await a;    // 8
            _ret_val = _ret_val + 1;
            _ret_end = 1;
        with
            await a;    // 12
            _ret_val = _ret_val + 2;
        end
    end
with
    await OS_START;
    emit a;
    emit a;
end
]],
    todo = 'no more ret_val/ret_end',
    _ana = {
        isForever = true,
        acc = 3,
        abrt  = 3,
    },
    awaits = 1,
    run = 1,
}

Test { [[
input void OS_START;
event int a, x, y;
var int ret = 0;
par do
    par/and do
        await OS_START;
        emit x => 1;   // 7
        emit y => 1;   // 8
    with
        par/or do
            await y;
            escape 1;   // 12
        with
            await x;
            escape 2;   // 15
        end;
    end;
with
    await OS_START;
    emit x => 1;       // 20
    emit y => 1;       // 21
end
]],
    _ana = {
        acc = 3,
        abrt = 5,   -- TODO: not checked
    },
    run = 2;
}

Test { [[
input void OS_START;
event void a, b;
par do
    par do
        await a;    // 5
        escape 1;   // 6
    with
        await b;
        escape 2;   // 9
    end
with
    await OS_START;    // 12
    emit b;
with
    await OS_START;    // 15
    emit a;
end
]],
    run = 2,
    _ana = {
        acc = 1,
        abrt = 5,
    },
}

Test { [[
native _V;
native do
    int V = 10;
end
event void a;
par/or do
    await 1s;
    emit a;
    _V = 1;
with
    await a;
end
await 1s;
escape _V;
]],
    run = { ['~>2s']=10 },
}
Test { [[
native _V;
native do
    int V = 10;
end
event void a;
par/or do
    await a;
with
    await 1s;
    emit a;
    _V = 1;
end
await 1s;
escape _V;
]],
    run = { ['~>2s']=10 },
}
Test { [[
input void OS_START;
event int e;
var int ret = 1;
par/or do
    do
        var int x = 2;
        par/or do
            await OS_START;
            emit e => x;
        with
            await e;
        end
    end
    do
        var int x = 10;
        await 1s;
        ret = x;
    end
with
    var int v = await e;
    ret = v;
end
escape ret;
]],
    run = { ['~>2s']=10 },
    --run = { ['~>2s']=2 },
}

Test { [[
input int A;
var int a=0, b=0;
par/and do
    a = await A;
    a = a + 1;
with
    b = await A;
    b = b+1+1;
end;
escape a + b;
]],
    run = { ['0~>A']=3, ['5~>A']=13 },
}

Test { [[
input int A,B;
var int a=0,b=0,c=0,d=0;
par/or do
    par/and do          // 4
        a = await A;
    with
        b = await B;
    end;
    c = 1;
with
    par/and do          // 11
        b = await B;
    with
        a = await A;
    end;
    d = 2;
end;
escape a + b + c + d;
]],
    _ana = {
        acc = 2,
        abrt = 5,   -- TODO: not checked
    },
    run = { ['0~>A;5~>B']=6 },
    --run = { ['0~>A;5~>B']=8 },
    --todo = 'nd excpt',
}

Test { [[
input int A,B;
var int a=0,b=0,ret=0;
par/and do
    await A;
    a = 1+2+3+4;
with
    var int v = await B;
    b = 100+v;
    ret = a + b;
end;
escape ret;
]],
    run = { ['1~>A;10~>B']=120 },
}

Test { [[
input int A,B;
var int a=0,b=0;
par/or do
    if true then
        a = await A;
    else
        b = await B;
    end;
with
    if true then
        b = await B;
    else
        a = await A;
    end;
end;
escape a + b;
]],
    _ana = {
        acc = 2,
        abrt = 5,   -- TODO: not checked
    },
    run = { ['1~>A;10~>B']=1 },
}

Test { [[
par do
    escape 1;
with
    escape 2;
end;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
input int A;
par do
    escape 1;
with
    await A;
    escape 1;
end;
]],
    _ana = {
        abrt = 1,
        --unreachs = 1,
    },
    run = 1,
}
Test { [[
input int A;
par do
    var int v = await A;
    escape v;
with
    var int v = await A;
    escape v;
end;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
    run = { ['1~>A']=1, ['2~>A']=2 },
}

Test { [[
par do
    await FOREVER;
with
    escape 10;
end;
]],
    _ana = {
        abrt = 1,
    },
    run = 10,
}

Test { [[
input int A,B,Z;
par do
    var int v = await A;
    escape v;
with
    var int v = await B;
    escape v;
with
    var int v = await Z;
    escape v;
end;
]],
    run = { ['1~>A']=1, ['2~>B']=2, ['3~>Z']=3 }
}
Test { [[
par/and do
with
end;
escape 1;
]],
    run = 1,
}
Test { [[
par/or do
with
end;
escape 1;
]],
    _ana = {
        abrt = 3,
    },
    run = 1,
}
Test { [[
input int A,B;
par do
    await A;
    var int v = await A;
    escape v;
with
    var int v = await B;
    escape v;
end;
]],
    run = {
        ['0~>B'] = 0,
        ['0~>A ; 3~>A'] = 3,
        ['0~>A ; 2~>B'] = 2,
    },
}

Test { [[
input int A,B;
await A;
par do
    var int v = await A;
    escape v;
with
    var int v = await B;
    escape v;
end;
]],
    run = {
        ['0~>B ; 0~>B ; 1~>A ; 3~>A'] = 3,
        ['0~>B ; 0~>B ; 1~>A ; 3~>B'] = 3,
    },
}
Test { [[
input int A,B,Z;
par do
    await A;
    var int v = await B;
    escape v;
with
    await A;
    var int v = await Z;
    escape v;
end;
]],
    run = {
        ['0~>B ; 0~>B ; 1~>A ; 3~>B'] = 3,
        ['0~>B ; 0~>B ; 1~>A ; 3~>Z'] = 3,
    },
}
Test { [[
input int A,B,Z;
await A;
par do
    var int v = await B;
    escape v;
with
    var int v = await Z;
    escape v;
end;
]],
    run = {
        ['0~>B ; 0~>B ; 1~>A ; 3~>B'] = 3,
        ['0~>B ; 0~>B ; 1~>A ; 3~>Z'] = 3,
    },
}

Test { [[
par/or do
    await 10s;
with
    await 10s;
end;
escape 1;
]],
    _ana = {
        abrt = 3,
    },
    run = {
        ['~>10s'] = 1,
        ['~>20s'] = 1,
    }
}
Test { [[
par do
    var int a = await 10ms;
    escape a;
with
    var int b = await 10ms;
    escape b;
end;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
    run = {
        ['~>10ms'] = 0,
        ['~>20ms'] = 10000,
    }
}
Test { [[
var int a=1;
par/or do
    a = await 10ms;
with
    a = await 10ms;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>10ms'] = 0,
        ['~>20ms'] = 10000,
    }
}
Test { [[
var int a=0,b=0;
par/or do
    await 10us;
    await 10us;
    a = 1;
with
    await 20us;
    b = 1;
end;
escape a + b;
]],
    _ana = {
        abrt = 4,
    },
    run = {
        ['~>20us'] = 1,
        --['~>20us'] = 2,
    },
}
Test { [[
var int a=0,b=0;
par/or do
    await (10)us;
    await (10)us;
    a = 1;
with
    await 20us;
    b = 1;
end;
escape a + b;
]],
    _ana = {
        abrt = 4,
    },
    run = {
        --['~>20us'] = 2,
        ['~>20us'] = 1,
    }
}
Test { [[
var int a=0,b=0;
par/or do
    await (10)us;
    await (10)us;
    a = 1;
with
    await (20)us;
    b = 1;
end;
escape a + b;
]],
    _ana = {
        abrt = 4,
    },
    run = {
        ['~>20us'] = 1,
        --['~>20us'] = 2,
    }
}
Test { [[
var int a=0,b=0;
par/or do
    await 10us;
    await 10us;
    a = 1;
with
    await (20)us;
    b = 1;
end;
escape a + b;
]],
    _ana = {
        abrt = 4,
    },
    run = {
        ['~>20us'] = 1,
        --['~>20us'] = 2,
    }
}
Test { [[
var int a=100,b=100;
par/or do
    a = await 10us;
with
    b = await (10)us;
end;
escape a + b;
]],
    _ana = {
        abrt = 3,
    },
    run = {
        --['~>10us'] = 0,
        ['~>10us'] = 100,
        --['~>20us'] = 20,
        ['~>20us'] = 110,
    }
}
Test { [[
var int a=0,b=0;
par do
    a = await 10ms;
    escape a;
with
    b = await (10000)us;
    escape b;
end;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
var int a=0,b=0;
par/or do
    a = await 10ms;
with
    await (5)ms;
    b = await (2)ms;
end;
escape a+b;
]],
    _ana = {
        abrt = 4,
    },
    run = {
        ['~>10ms'] = 3000,
        ['~>20ms'] = 13000,
    }
}
Test { [[
var int a=0,b=0;
par/or do
    a = await 10ms;
with
    await (5)ms;
    b = await 2ms;
end;
escape a+b;
]],
    _ana = {
        abrt = 4,
    },
    run = {
        ['~>10ms'] = 3000,
        ['~>20ms'] = 13000,
    }
}
Test { [[
var int a=0,b=0;
par do
    a = await 10us;
    escape a;
with
    b = await (5)us;
    await 5us;
    escape b;
end;
]],
    _ana = {
        acc = 1,
        abrt = 4,
    },
}
Test { [[
var int a=0,b=0;
par do
    a = await 10us;
    escape a;
with
    b = await (5)us;
    await 10us;
    escape b;
end;
]],
    _ana = {
        acc = 1,     -- TODO: =0 (await(5) cannot be 0)
        abrt = 4,
    },
}

Test { [[
input void A;
var int v1=0, v2=0;
par/or do
    await 1s;           // 4
    v1 = v1 + 1;
with
    loop do
        par/or do
            await 1s;   // 9
        with
            await A;
        end
        v2 = v2 + 1;
    end
end
escape v1 + v2;
]],
    _ana = {
        abrt = 1,
    },
    run = { ['~>A;~>1ms;~>A;~>1ms;~>A;~>1ms;~>A;~>1ms;~>A;~>1ms;~>1s']=6 }
}

Test { [[
input void A;
var int v1=0, v2=0, v3=0;
par/or do
    await 1s;           // 4
    v1 = v1 + 1;
with
    loop do
        par/or do
            await 1s;   // 9
        with
            await A;
        end
        v2 = v2 + 1;
    end
with
    loop do
        par/or do
            await 1s;   // 18
        with
            await A;
            await A;
        end
        v3 = v3 + 1;
    end
end
escape v1 + v2 + v3;
]],
    _ana = {
        abrt = 2,
    },
    run = { ['~>A;~>1ms;~>A;~>1ms;~>A;~>1ms;~>A;~>1ms;~>A;~>1s']=8 }
}

Test { [[
par do
    loop do
        loop do
            await 1s;
        end
    end
with
    loop do
        await 500ms;
        async do
        end
    end
end
]],
    _ana = {
        isForever = true,
        unreachs = 1,
    }
}
Test { [[
par do
    loop do
        await 1s;
        loop do
            await 1s;
        end
    end
with
    loop do
        await 1s;
        async do
        end
    end
end
]],
    _ana = {
        isForever = true,
        unreachs = 1,
    }
}

Test { [[
var int v=0;
par/or do
    loop do         // 3
        break;      // 4
    end
    v = 2;
with
    v = 1;          // 8
end
escape v;
]],
    _ana = {
        unreachs = 1,
        acc = 1,
        abrt = 4,
    },
}

Test { [[
var int v = 1;
loop do
    par/or do
        break;
    with
        v = v + 1;
        await 1s;
    end
    v = v * 2;
end
escape v;
]],
    run = 1,
    --run = 2,
    _ana = {
        abrt = 1,
        --unreachs = 3,
    },
}

Test { [[
input int A;
var int a=0;
loop do
    par/or do
        await 10ms;
        a = 1;
        break;
    with
        a = 1;
        await A;
    end
end
escape 0;
]],
    run = 0,
}
Test { [[
input void A,B;
var int a=0;
loop do
    par/or do
        await B;
        a = 2;
    with
        a = 1;
        await A;
    end
end
]],
    _ana = {
        isForever = true,
    },
}
Test { [[
input int A;
var int a=0;
loop do
    par/or do
        loop do
            await (10)us;
            await 10ms;
            if true then
                a = 1;      // 9
                break;
            else
                a = 0;
            end
        end
    with
        loop do
            a = 1;          // 15
            await A;
        end
    end
end
]],
    _ana = {
        isForever = true,
    },
}
Test { [[
input int A;
var int a=1;
loop do
    par/or do
        loop do
            await (10)us;
            await 10ms;
            if true then
                a = 1;      // 9
                break;
            end
        end
    with
        loop do
            a = 1;          // 15
            await A;
        end
    end
end
]],
    safety = 2,
    _ana = {
        acc = 1,
        isForever = true,
    },
}
Test { [[
input int A;
var int a=0;
loop do
    par/or do
        loop do
            await 10ms;
            await (10)us;
            if true then
                break;
            end;
        end;
    with
        loop do
            await A;
        end;
    end;
end;
]],
    _ana = {
        isForever = true,
    },
}
Test { [[
var int v=0;
par/or do
    await 10ms;
    v = 10;
with
    await (1)ms;
    await 10ms;
    v = 0;
end
escape v;
]],
    todo = 'acc should be 0',
    simul = {
        unreachs = 1,
    },
    run = 10,
}

Test { [[
var int a=0;
loop do
    par/or do
        loop do             // 4
            await (10)us;
            await 10ms;
            if true then
                break;
            end;
        end;
        a = 1;
    with
        loop do
            await 10ms;     // 14
            a = 1;
        end;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,
        abrt = 1,
    },
}
Test { [[
loop do
    await 10ms;
    await (10)us;
    if true then
        break;
    end;
end;
escape 0;
]],
    run = { ['~>20ms'] = 0 }
}
Test { [[
par do
    loop do
        await (20)ms;
    end;
with
    loop do
        await 20ms;
    end;
end;
]],
    _ana = {
        isForever = true
    },
}
Test { [[
var int a=0;
par/or do
    loop do             // 3
        await 10ms;
        await (10)us;
        if true then
            break;
        end;
    end;
    a = 1;
with
    loop do
        await 100ms;    // 13
        a = 1;
    end;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 1,
    },
    run = { ['~>11ms']=1 },
}
Test { [[
input int A;
loop do
    par/or do
        await 10ms;
        await A;
    with
        await 20ms;
    end;
end;
]],
    _ana = {
        abrt = 1,
        isForever = true,
    },
}
Test { [[
var int a=0;
loop do
    par/or do
        loop do
            await 10ms;
            await (10)us;
            if true then
                break;
            end;
        end;
        a = 1;
    with
        loop do
            await 10ms;
            a = 1;
        end;
    end;
end;
]],
    _ana = {
        abrt = 1,
        isForever = true,
        acc = 1,
    },
}
Test { [[
var int a=0;
loop do
    par/or do
        loop do
            await (10)us;
            await 10ms;
            if true then
                break;
            end;
        end;
        a = 1;
    with
        loop do
            await 100ms;
            a = 1;
        end;
    end;
end;
]],
    _ana = {
        abrt = 1,
        isForever = true,
        acc = 1,
    },
}
Test { [[
var int a=0,b=0;
par/or do
    a = await 10ms;
    escape a;
with
    b = await (5)us;
    await 11ms;
    escape b;
end;
]],
    todo = 'await(x) pode ser <0?',  -- TIME_undef
    _ana = {
        acc = 1,
    },
}
Test { [[
var int a=0,b=0;
par do
    a = await 10ms;
    escape a;
with
    b = await (10000)us;
    escape b;
end;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>10ms'] = 0,
        ['~>20ms'] = 10000,
    }
}
Test { [[
var int a=0,b=0;
par/and do
    a = await 10us;
with
    b = await (9)us;
end;
escape a+b;
]],
    run = {
        ['~>10us'] = 1,
        ['~>20us'] = 21,
    }
}
Test { [[
var int a=0,b=0,c=0;
par do
    a = await 10us;
    escape a;
with
    b = await (9)us;
    escape b;
with
    c = await (8)us;
    escape c;
end;
]],
    _ana = {
        acc = 3,
        abrt = 9,
    },
}
Test { [[
var int a=0,b=0,c=0;
par/or do
    a = await 10us;
with
    b = await (9)us;
with
    c = await (8)us;
end;
escape a+b+c;
]],
    _ana = {
        abrt = 9,
    },
    run = {
        ['~>10us'] = 2,
        ['~>20us'] = 12,
    }
}
Test { [[
var int a=0,b=0,c=0;
par/and do
    a = await 10ms;
with
    b = await (9000)us;
with
    c = await (8000)us;
end;
escape a+b+c;
]],
    run = {
        ['~>10ms'] = 3000,
        ['~>20ms'] = 33000,
    }
}
Test { [[
var int a=0,b=0,c=0;
par do
    a = await 10us;
    escape a;
with
    b = await (10)us;
    escape b;
with
    c = await 10us;
    escape c;
end;
]],
    _ana = {
        abrt = 9,
        acc = 3,
    },
}
Test { [[
var s32 a=0,b=0;
par do
    a = await 10min;
    escape a;
with
    b = await 20min;
    escape b;
end;
]],
    _ana = {
        --unreachs = 1,
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>10min']  = 0,
        ['~>20min']  = 600000000,
    }
}
Test { [[
await 35min;
escape 0;
]],
    sval = 'line 1 : constant is out of range',
}
Test { [[
var int a = 2;
par/or do
    await 10s;
with
    await 20s;
    a = 0;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        --unreachs = 1,
    },
    run = {
        ['~>10s'] = 2,
        ['~>20s'] = 2,
        ['~>30s'] = 2,
    }
}
Test { [[
var int a = 2;
par/or do
    await (10)us;
with
    await 20ms;
    a = 0;
end;
escape a;
]],
    _ana = {
        abrt = 3,
    },
    run = {
        ['~>10ms'] = 2,
        ['~>20ms'] = 2,
        ['~>30ms'] = 2,
    }
}
Test { [[
var int a = 2;
par/or do
    var int b = await (10)us;
    a = b;
with
    await 20ms;
    a = 0;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
var s32 v1=0,v2=0;
par do
    v1 = await 5min;
    escape v1;
with
    await 1min;
    v2 = await 4min;
    escape v2;
end;
]],
    _ana = {
        acc = 1,
        abrt = 4,
    },
    run = {
        ['~>1min ; ~>1min ; ~>1min ; ~>1min ; ~>1min'] = 0,
        ['~>2min ; ~>4min'] = 60000000,
        ['~>4min ; ~>1min'] = 0,
    }
}

Test { [[
input int A;
loop do
    await 10ms;
    await A;
    await 10ms;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
input int A;
loop do
    await A;
    await 10ms;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
input int A;
loop do
    await A;
    await 10ms;
    await A;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
input int A;
loop do
    await 10ms;
    await A;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
loop do
    await 10ms;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
input int A;
async do
    emit A;
end
escape 1;
]],
    env = 'line 3 : arity mismatch',
    --env = 'line 3 : missing parameters on `emit´',
}

Test { [[
input void C;
var int a=0;
par do
    await 5s;
    await FOREVER;
with
    a = 0;
    loop do
        await 1s;
        a = a + 1;
    end;
with
    await C;
    escape a;
end;
]],
    run = { ['~>10s;~>C']=10 }
}

Test { [[
input void C;
do
    var int a=0, b=0, c=0;
    par do
        loop do
            await 10ms;
            a = a + 1;
        end;
    with
        loop do
            await 100ms;
            b = b + 1;
        end;
    with
        loop do
            await 1000ms;
            c = c + 1;
        end;
    with
        await C;
        escape a + b + c;
    end;
end;
]],
    run = {
        ['~>999ms; ~>C'] = 108,
        ['~>5s; ~>C'] = 555,
        ['~>C'] = 0,
    }
}

Test { [[
input void C;
do
    var int a=0, b=0, c=0;
    par do
        loop do
            await 10ms;
            a = a + 1;
        end;
    with
        loop do
            await 100ms;
            b = b + 1;
        end;
    with
        loop do
            await 1000ms;
            c = c + 1;
        end;
    with
        await C;
        escape a + b + c;
    end;
end;
]],
    run = {
        ['~>999ms; ~>C'] = 108,
        ['~>5s; ~>C'] = 555,
        ['~>C'] = 0,
    },
    safety = 2,
    _ana = {
        acc = 3,
    },
}

    -- TIME LATE

Test { [[
var int a, b;
(a,b) = await 1s;
escape 1;
]],
    env = 'line 2 : arity mismatch',
    --gcc = 'error: ‘tceu__s32’ has no member named ‘_2’',
    --run = 1,
}

Test { [[
input int C;
var int late = 0;
var int v=0;
par do
    loop do
        v = await 1ms;
        late = late + v;
    end;
with
    await C;
    escape late;
end;
]],
    run = {
        ['~>1ms; ~>1ms; ~>1ms; ~>1ms; ~>1ms; 1~>C'] = 0,
        ['~>1ms; ~>1ms; ~>1ms; ~>10ms; 1~>C'] = 45000,
        ['~>1ms; ~>1ms; ~>2ms; 1~>C'] = 1000,
        ['~>2ms; 1~>C'] = 1000,
        ['~>2ms; ~>2ms; 1~>C'] = 2000,
        ['~>4ms; 1~>C'] = 6000,
        ['1~>C'] = 0,
    }
}

Test { [[
input int A;
par do
    var int v = await A;
    escape v;
with
    var int v = await (1)us;
    escape v;
end;
]],
    run = {
        ['~>10us'] = 9,
        ['10~>A'] = 10,
    }
}

Test { [[
var int v=0;
par/or do
    v = await 10us;
with
    v = await (1)us;
end;
escape v;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>1us'] = 0,
        ['~>20us'] = 19,
    }
}

Test { [[
input int A;
var int a=0;
par/or do
    a = await A;
with
    a = await (1)us;
end;
escape a;
]],
    run = {
        ['~>10us'] = 9,
        ['10~>A'] = 10,
    }
}

Test { [[
input int A;
var int a=0;
par/or do
    a = await 30us;
with
    a = await A;
end;
escape a;
]],
    run = {
        ['~>30us'] = 0,
        ['~>60us'] = 30,
        ['10~>A'] = 10,
    }
}

-- 1st to test timer clean
Test { [[
input int A, C;
var int a=0;
par/or do
    a = await 10min;
with
    a = await A;
end;
await C;
escape a;
]],
    run = {
        ['1~>A  ; 1~>C'] = 1,
        ['~>10min ; 1~>C'] = 0,
        ['~>10min ; 1~>A ; 1~>C'] = 0,
        ['1~>A  ; ~>10min; 1~>C'] = 1,
    }
}

Test { [[
native do ##include <assert.h> end
native _assert;
input void A;
var int ret = 0;
par/or do
    loop do
        var int late = await 10ms;
        ret = ret + late;
        _assert(late <= 10000);
    end
with
    loop do
        var int i = 0;
        var int t=0;
        par/or do
            t = await 1s;
        with
            loop do
                await A;
                i = i + 1;
            end
        end
        if t then end;
    end
with
    async do
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
    end
end
escape ret;
]],
    run = 72000,
}

Test { [[
input void OS_START;
event int a;
var int ret = 1;
par/or do
    await OS_START;
    emit a => 10;
with
    ret = await a;
end;
escape ret;
]],
    _ana = {
        excpt = 1,
    },
    run = 10,
}

Test { [[
event int a;
var int ret = 1;
par/or do
    emit a => 10;
with
    ret = await a;
end;
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 1,
    },
    run = 1,
}

Test { [[
input void OS_START;
event int a;
var int ret = 1;
par/and do
    await OS_START;
    emit a => 10;
with
    ret = await a;
end;
escape ret;
]],
    _ana = {
        --acc = 1,
    },
    run = 10,
}
-- TODO: STACK
Test { [[
event int a;
par/and do
    await a;
with
    emit a => 1;
end;
escape 10;
]],
    _ana = {
        acc = 1,
    },
    --run = 10,
    run = 0,
}

Test { [[
input int A;
event int b, c;
par do
    await A;
    emit b => 1;
    await c;        // 6
    escape 10;      // 7
with
    await b;
    await A;
    emit c => 10;      // 11
end;
]],
    _ana = {
        isForever = false,
        --unreachs = 2,
        --nd_esc = 1,
        acc = 1,
    },
    run = {
        ['0~>A ; 0~>A'] = 10,
    }
}

Test { [[
input int A;
event int b, c;
par do
    await A;
    emit b => 1;
    await c;        // 6
    escape 10;      // 7
with
    await b;
    await A;
    emit c => 10;      // 11
    // unreachable
    await c;
    // unreachable
    escape 0;       // 15
end;
]],
    _ana = {
        isForever = false,
        --unreachs = 2,
        --nd_esc = 1,
        acc = 1,
    },
    run = {
        ['0~>A ; 0~>A'] = 10,
    }
}

Test { [[
input int A;
var int a = 1;
loop do
    par/or do
        a = await A;
    with
        a = await A;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,
        abrt = 3,
    },
}
Test { [[
event int a;
par/or do
    escape 1;       // TODO: [false]=true
with
    emit a => 1;       // TODO: elimina o [false]
    // unreachable
end;
// unreachable
await a;
// unreachable
escape 0;
]],
    _ana = {
        abrt = 3,
        --unreachs = 3,
    },
    run = 1,
    --trig_wo = 1,
}
Test { [[
event int a;
par/or do
with
    emit a => 1;
    // unreachable
end;
// unreachable
await a;
// unreachable
escape 0;
]],
    _ana = {
        abrt = 3,
        --unreachs = 2,
        --isForever = true,
    },
    --dfa = 'unreachable statement',
    --trig_wo = 1,
}
Test { [[
event int a;
par do
    escape 1;
with
    emit a => 1;
    // unreachable
end;
]],
    _ana = {
        --unreachs = 1,
        --nd_esc = 1,
        abrt = 1,
    },
    run = 1,
    --trig_wo = 1,
}
Test { [[
event int a;
par do
    emit a => 1;
    escape 1;
with
    escape 2;
end;
]],
    _ana = {
        --unreachs = 1,
        acc = 1,
        abrt = 3,
        --trig_wo = 1,
    },
    run = 1,
}
Test { [[
event int a;
par/or do
    emit a => 1;
with
end;
await a;
escape 0;
]],
    _ana = {
        --unreachs = 2,
        abrt = 3,
        --isForever = true,
    },
    --trig_wo = 1,
}

Test { [[
var int v1=2,v2=3;
par/or do
with
end
escape v1+v2;
]],
    run = 5,
}
Test { [[
var int v1,v2;
par/or do
with
end
v1=2;
v2=3;
escape v1+v2;
]],
    ref = 'line 1 : uninitialized variable "v1" crossing compound statement (tests.lua:2)',
}
Test { [[
par/or do
with
end
var int v1,v2;
v1=2;
v2=3;
escape v1+v2;
]],
    run = 5,
}
Test { [[
var int v1=0,v2=0;
do
    par/or do
    with
    end
    v1=2;
    v2=3;
end
escape v1+v2;
]],
    run = 5,
}

Test { [[
var int v1=0,v2=0;
par/and do
    v1 = 3;
with
    v2 = 2;
end
escape v1+v2;
]],
    run = 5,
}

Test { [[
event int a;
var int v1=0,v2=0;
par/or do
    emit a => 2;
    v1 = 3;
with
    v2 = 2;
end
escape v1+v2;
]],
    _ana = {
        abrt = 3,
        --unreachs = 1,
        --nd_esc = 1,
    },
    --run = 4,        -- TODO: stack change
    run = 3,
}

Test { [[
event int a;
var int v1=0,v2=0,v3=0;
par/or do
    emit a => 2;
    v1 = 2;
with
    v2 = 2;
with
    await a;
    v3 = 2;
end
escape v1+v2+v3;
]],
    _ana = {
        --unreachs = 2,
        acc = 1,
        abrt = 5,
    },
    --run = 4,        -- TODO: stack change
    run = 2,
}

Test { [[
event int a;
var int v1=0,v2=0,v3=0;
par/or do
    emit a => 2;
    v1 = 2;
with
    await a;
    v3 = 2;
with
    v2 = 2;
end
escape v1+v2+v3;
]],
    _ana = {
        --unreachs = 2,
        acc = 1,
        abrt = 5,
    },
    --run = 4,        -- TODO: stack change
    run = 2,
}

Test { [[
var int ret = 0;
par/or do
    ret = 1;
with
    ret = 2;
end
ret = ret * 2;
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = 2,
}

-- 1st to escape and terminate
Test { [[
event int a;
var int ret=9;
par/or do
    par/or do
        emit a => 2;
    with
        ret = 3;
    end;
with
    var int aa = await a;
    ret = aa + 1;
end;
escape ret;
]],
    _ana = {
        --unreachs = 2,
        abrt = 4,
        acc = 1,
    },
    --run = 3,
    run = 9,
}

-- 1st to escape and terminate
-- TODO: STACK
Test { [[
event int a;
var int ret=9;
par/or do
    var int aa = await a;
    ret = aa + 2;
with
    par/or do
        emit a => 2;
    with
        ret = 3;
    end;
end;
escape ret;
]],
    _ana = {
        --unreachs = 2,
        abrt = 4,
        acc = 1,
    },
    run = 9,
    --run = 4,
}

Test { [[
input int A;
var int a=0;
par do
    a = await A;
    escape a;
with
    a = await A;
    escape a;
end;
]],
    _ana = {
        acc = 4,
        abrt = 3,
    },
    run = { ['5~>A']=5 },
}
Test { [[
input int A;
var int a=0;
par/or do
    a = await A;
with
    await A;
end;
escape a;
]],
    _ana = {
        abrt = 3,
    },
    run = {
        ['1~>A'] = 1,
        ['2~>A'] = 2,
    },
    --todo = 'nd excpt',
}
Test { [[
input int A;
var int a=10;
par/or do
    await A;
with
    a = await A;
end;
escape a;
]],
    _ana = {
        abrt = 3,
    },
    run = {
        --['1~>A'] = 1,
        --['2~>A'] = 2,
        ['1~>A'] = 10,
        ['2~>A'] = 10,
    }
}
Test { [[
input int A;
var int a=0;
par/or do
    await A;
    a = 10;
with
    await A;
    var int v = a;
    if v then end;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['1~>A'] = 10,
        ['2~>A'] = 10,
    },
}

Test { [[
input int A;
var int a=0;
par/or do
    await A;
    a = 10;
with
    await A;
    a = 11;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}

Test { [[
input int A;
var int a=0;
par/or do
    await A;
    a = 10;
with
    await A;
    escape a;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}

Test { [[
input int A;
loop do
    par/or do
        await A;    // 4
    with
        await A;    // 6
        if true then
            break;  // 8
        end;
    end;
end;
escape 0;
]],
    _ana = {
        abrt = 5,
    },
    run = 0,
}

Test { [[
input int A;
loop do
    loop do
        par/or do
            await 1s;
        with
            if false then
                await A;
                break;
            end;
        end;
        await FOREVER;
    end;
end;
]],
    _ana = {
        abrt = 1,
        unreachs = 1,
        isForever = true,
    },
}

Test { [[
input int A,B;

loop do
    par/or do
        await B;
    with
        await A;
        if true then
            break;
        end;
    end;
end;

par do
    loop do
        await A;
    end;

with
    loop do
        await 2s;
    end;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
input int A;
var int a = par do
    await A;
    var int v = 10;
    escape a;
with
    await A;
    escape a;
end;
escape a;
]],
    todo = '"a"s deveriam ser diferentes',
    _ana = {
        acc = 1,
        abrt = 1,
    },
}

Test { [[
input void A,B;
var int a = 5;
par/or do
    par/or do
        await A;
    with
        await B;
    end;
    a = 10;
with
    await A;
    escape a;
end;
escape a;
]],
    run = { ['~>A']=10, ['~>B']=10 },
    _ana = {
        acc = 1,
        abrt = 3,
    },
}

Test { [[
input void A,B;
var int a = 5;
par/or do
    par/and do
        await A;
    with
        await B;
    end;
    a = 10;
with
    await A;
    escape a;
end;
escape a;
]],
    run = { ['~>A']=5, ['~>B;~>A']=10 },
    _ana = {
        acc = 1,
        abrt = 3,
    },
}

Test { [[
input int A,B;
var int a = 0;
par/or do
    par/or do
        var int v = await A;
        escape v;
    with
        await B;
    end;
    a = 10;
with
    await A;
end;
escape a;
]],
    _ana = {
        abrt = 3,
    },
    run = {
        ['1~>B'] = 10,
        --['2~>A'] = 1,
    }
}

Test { [[
input int A, Z;
var int v=0;
loop do
    par/or do
        await A;
    with
        v = await Z;
        break;
    end;
end;
escape v;
]],
    run = {
        ['0~>A ; 0~>A ; 3~>Z'] = 3,
        ['0~>A ; 0~>A ; 4~>Z'] = 4,
    }
}
Test { [[
input int A,B,Z;
var int v=0;
loop do
    par/or do
        await A;
        await B;
    with
        v = await Z;
        break;
    end;
end;
escape v;
]],
    run = {
        ['0~>A ; 0~>A ; 3~>Z'] = 3,
        ['0~>A ; 0~>A ; 0~>B ; 1~>B ; 4~>Z'] = 4,
    }
}
Test { [[
input int A,B,Z;
var int v=0;
loop do
    par/or do
        await A;
        await B;
    with
        v = await Z;
        break;
    end;
end;
escape v;
]],
    run = {
        ['0~>A ; 0~>A ; 3~>Z'] = 3,
        ['0~>A ; 0~>A ; 0~>B ; 1~>B ; 4~>Z'] = 4,
    }
}
Test { [[
input int A,B;
var int v=0;
loop do
    par do
        await A;
        await B;
    with
        v = await A;
        break;
    end;
end;
escape v;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = {
        ['0~>B ; 0~>B ; 3~>A'] = 3,
    }
}
Test { [[
input int A,B;
var int v=0;
loop do
    par/or do
        await A;
        await B;
    with
        v = await A;
        break;
    end;
end;
escape v;
]],
    _ana = {
        --unreachs = 3,
        --dfa = 'unreachable statement',
        abrt = 1,
    },
    run = {
        ['0~>B ; 0~>B ; 3~>A'] = 3,
    }
}
Test { [[
input int A, B;
var int v=0;
loop do
    par/or do
        v = await A;
        v = await A;
        break;
    with
        v = await A;
        await B;
    end;
end;
escape v;
]],
    _ana = {
        acc = 2,     -- TODO: should be 0
        abrt = 1,
    },
    run = {
        ['0~>B ; 0~>A ; 0~>B ; 0~>A ; 3~>A'] = 3,
    }
}
Test{ [[
input int A;
var int v=0;
loop do
    v = await A;
    break;
end;
escape v;
]],
    _ana = {
        unreachs = 1,
    },
    run = {
        ['1~>A'] = 1,
        ['2~>A'] = 2,
    }
}

Test { [[
input int A;
var int a=0;
par/and do
    a = await 30ms;
with
    await A;
end;
escape a;
]],
    run = {
        ['~>30ms ; 0~>A'] = 0,
        ['0~>A   ; ~>30ms'] = 0,
        ['~>60ms ; 0~>A'] = 30000,
        ['0~>A   ; ~>60ms'] = 30000,
    }
}

Test { [[
input int A,B;
par/and do
    await A;
with
    await B;
    await A;
end;
escape 1;
]],
    run = {
        ['1~>A ; 0~>B ; 0~>B ; 1~>A'] = 1,
    }
}
Test { [[
input int A;
par/and do
    await 30ms;
with
    await A;
    await 30ms;
end;
escape 1;
]],
    run = {
        ['~>30ms ; 0~>A ; ~>50ms'] = 1,
        ['0~>A ; ~>40ms'] = 1,
        ['0~>A ; ~>20ms ; ~>20ms'] = 1,
    }
}
Test { [[
input int A;
par/and do
    await 30ms;
with
    await A;
    await (30)us;
end;
escape 1;
]],
    run = {
        ['~>30ms ; 0~>A ; ~>50ms'] = 1,
        ['0~>A ; ~>40ms'] = 1,
        ['0~>A ; ~>20ms ; ~>20ms'] = 1,
    }
}

Test { [[
input void OS_START;
event int a,b,c;
var int cc = 1;
par/and do
    await OS_START;
    emit b => 1;
    emit c => 1;
with
    await b;
    par/or do
    with
        par/or do
        with
            cc = 5;
        end;
    end;
end;
escape cc;
]],
    _ana = {
        abrt = 6,   -- TODO: not checked
    },
    run = 1,
}

Test { [[
input int A;
var int a=0;
par/and do
    await 30ms;
    a = 1;
with
    await A;
    await 30ms;
    a = 2;
end;
escape a;
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['~>30ms ; 0~>A ; ~>50ms'] = 2,
        ['~>1ms ; 0~>A ; ~>40ms'] = 2,
        ['~>1ms ; 0~>A ; ~>20ms ; ~>20ms'] = 2,
    }
}

-- tests AwaitT after Ext
Test { [[
input int A;
var int a=0;
par/and do
    await A;
    await 30ms;
    a = 2;
with
    await 30ms;
    a = 1;
end;
escape a;
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['~>30ms ; 0~>A ; ~>50ms'] = 2,
        ['~>1us; 0~>A ; ~>40ms'] = 2,
        ['~>1us; 0~>A ; ~>20ms ; ~>20ms'] = 2,
    }
}

Test { [[
input int A;
var int a=0;
par/and do
    await 30ms;
    a = 1;
with
    await A;
    await 30ms;
    a = 2;
end;
escape a;
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['~>30ms ; 0~>A ; ~>50ms'] = 2,
        ['~>1ms ; 0~>A ; ~>40ms'] = 2,
        ['~>1ms ; 0~>A ; ~>20ms ; ~>20ms'] = 2,
    }
}

Test { [[
input void A;
var int a=0;
par/and do
    await A;
    await A;
    await 100ms;
    a = 1;
with
    await A;
    await A;
    await 10ms;
    await A;
    await 90ms;
    a = 2;
end;
escape a;
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['~>A ; ~>A ; ~>12ms; ~>A; ~>91ms'] = 2,
    }
}

Test { [[
input int A;
var int dt=0;
par/or do
    dt = await 20ms;
with
    await A;
    dt = await 20ms;
end;
escape dt;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>30ms'] = 10000,
        ['0~>A ; ~>40ms'] = 20000,
        ['~>10ms ; 0~>A ; ~>40ms'] = 30000,
    }
}
Test { [[
input int A;
var int dt=0;
par/or do
    await A;
    dt = await 20ms;
with
    dt = await 20ms;
end;
escape dt;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
    run = {
        ['~>30ms'] = 10000,
        ['0~>A ; ~>40ms'] = 20000,
        ['~>10ms ; 0~>A ; ~>40ms'] = 30000,
    }
}
Test { [[
input int A;
var int dt=0;
par/or do
    dt = await 20us;
with
    await A;
    dt = await 10us;
end;
escape dt;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
    run = {
        ['~>30us'] = 10,
        ['~>1us ; 0~>A ; ~>12us'] = 2,
        ['~>1us ; 0~>A ; ~>13us'] = 3,
    }
}
Test { [[
input int A;
var int dt=0;
par/or do
    dt = await 20ms;
with
    dt = await 10ms;
    await A;
    dt = await 10ms;
end;
escape dt;
]],
    _ana = {
        --unreachs = 1,
        acc = 2,
        abrt = 4,
    },
    run = {
        ['~>30ms'] = 10000,
        ['~>12ms ; 0~>A ; ~>8ms'] = 0,
        ['~>15ms ; 0~>A ; ~>10ms'] = 5000,
    }
}

Test { [[
input int A;
var int dt=0;
par do
    dt = await 20us;
    escape 1;
with
    dt = await 10us;
    await A;
    dt = await 10us;
    escape 2;
end;
]],
    _ana = {
        --unreachs = 1,
        abrt = 4,
        acc = 3,
    },
    run = {
        ['~>30us'] = 1,
        ['~>12us ; 0~>A ; ~>8us'] = 1,
        ['~>15us ; 0~>A ; ~>10us'] = 1,
    }
}

Test { [[
input int A,B;
var int ret=0;
par/or do
    await A;
    await 20ms;
    ret = 1;
with
    await B;
    await 20ms;
    ret = 2;
end;
escape ret;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
    run = {
        ['~>1us; 1~>A;~>25ms'] = 1,
        ['~>1us; 1~>A;~>1us; 1~>B;~>25ms'] = 1,
        ['~>1us; 1~>B;~>25ms'] = 2,
        ['~>1us; 1~>B;~>1us; 1~>A;~>25ms'] = 2,
    }
}

Test { [[
input int A,B;
var int ret=0;
par/or do
    await B;
    await 20ms;
    ret = 2;
with
    await A;
    await 20ms;
    ret = 1;
end;
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>1us; 1~>A;~>25ms'] = 1,
        ['~>1us; 1~>A;~>1us; 1~>B;~>25ms'] = 1,
        ['~>1us; 1~>B;~>25ms'] = 2,
        ['~>1us; 1~>B;~>1us; 1~>A;~>25ms'] = 2,
    }
}

Test { [[
input int A;
var int dt=0;
par/or do
    dt = await 20ms;
with
    await A;
    await 10ms;
    dt = await 10ms;
end;
escape dt;
]],
    _ana = {
        acc = 1,
        abrt = 4,
        --unreachs = 1,
    },
    run = {
        ['~>30ms'] = 10000,
        ['~>12ms ; 0~>A ; ~>8ms'] = 0,
        ['~>15ms ; 0~>A ; ~>10ms'] = 5000,
    }
}

Test { [[
input int A,B;
var int dt=0;
par/or do
    await A;
    dt = await 20ms;
with
    await B;
    dt = await 20ms;
end;
escape dt;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>30ms ; 0~>A ; ~>21ms'] = 1000,
        ['~>12ms ; 0~>A ; ~>1us; 0~>B ; ~>27ms'] = 7001,
        ['~>12ms ; 0~>B ; ~>3ms ; 0~>A ; ~>20ms'] = 3000,
    }
}

Test { [[
input int A,B;
var int dt=0;
par/or do
    await A;
    dt = await 20ms;
with
    await B;
    dt = await (20)ms;
end;
escape dt;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>30ms ; 0~>A ; ~>21ms'] = 1000,
        ['~>12ms ; 0~>A ; ~>1ms ; 0~>B ; ~>27ms'] = 8000,
        ['~>12ms ; 0~>B ; ~>3ms ; 0~>A ; ~>20ms'] = 3000,
    }
}

Test { [[
input int A,B;
var int dt=0;
var int ret = 10;
par/or do
    await A;
    dt = await 20ms;
    ret = 1;
with
    await B;
    dt = await (20)ms;
    ret = 2;
end;
escape ret;
]],
    _ana = {
        acc = 2,
        abrt = 3,
    },
    run = {
        ['~>30ms ; 0~>A ; ~>25ms'] = 1,
        ['~>12ms ; 0~>A ; ~>1ms ; 0~>B ; ~>27ms'] = 1,
        ['~>12ms ; 0~>B ; ~>3ms ; 0~>A ; ~>20ms'] = 2,
    }
}

Test { [[
input int A, B;
var int dt=0;
var int ret = 10;
par/or do
    await A;
    dt = await 20ms;
    ret = 1;
with
    await B;
    dt = await 20ms;
    ret = 2;
end;
escape ret;
]],
    _ana = {
        acc = 2,
        abrt = 3,
    },
    run = {
        ['~>12ms ; 0~>A ; ~>1ms ; 0~>B ; ~>27ms'] = 1,
        ['~>12ms ; 0~>B ; ~>1ms ; 0~>A ; ~>26ms'] = 2,
    }
}

-- Boa comparacao de unreachs vs abrt para timers
Test { [[
var int dt=0;
par/or do
    await 10ms;
    dt = await 10ms;
with
    dt = await 30ms;
end;
escape dt;
]],
    _ana = {
        acc = 1,
        abrt = 4,
        --unreachs = 1, -- apos ~30
    },
    run = {
        ['~>12ms ; ~>17ms'] = 9000,
    }
}
Test { [[
var int dt=0;
par/or do
    await 10us;
    dt = await (10)us;
with
    dt = await 30us;
end;
escape dt;
]],
    _ana = {
        acc = 1,
        abrt = 4,
    },
    run = {
        ['~>12us ; ~>17us'] = 9,
    }
}

Test { [[
input int A,B;
var int ret=0;
par/or do
    await A;
    await 10ms;
    ret = 0;
with
    await B;
    await 10ms;
    ret = 1;
end;
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['0~>A ; ~>1ms ; 0~>B ; ~>21ms'] = 0,
        ['0~>B ; ~>1ms ; 0~>A ; ~>21ms'] = 1,
        ['0~>A ; ~>1ms ; 0~>B ; ~>21ms'] = 0,
        ['0~>B ; ~>1ms ; 0~>A ; ~>21ms'] = 1,
    }
}

Test { [[
event int a, b;
var int x=0;
par/or do
    await a;                // 4
    await 10ms;             // 5
    x = 0;
with
    var int bb = await b;   // 8
    emit a => bb;              // 9
    await 10ms;
    x = 1;
with
    emit b => 1;       // 13
    x = 2;
    await FOREVER;
end;
escape x;
]],
    _ana = {
        abrt = 3,
        acc  = 2,    -- TODO: timer kills timer
        unreachs = 0,    -- TODO: timer kills timer
    },
    --run = { ['~>10ms']=0 },
}

Test { [[
event int a, b;
var int x=0;
var int bb=0;
par/or do
    await a;
    await 10ms;
    x = 0;
with
    bb = await b;
    await 10ms;
    x = 1;
with
    emit b => 1;
    emit a => bb;
    x = 2;
    await FOREVER;
end;
escape x;
]],
    _ana = {
        abrt = 3,
        acc = 3,     -- TODO: timer kills timer
        unreachs = 0,    -- TODO: timer kills timer
    },
    --run = { ['~>10ms']=0 },   -- TODO: intl timer
}

Test { [[
event int a, b;
var int x=0;
par/or do
    await a;
    await 10ms;
    x = 1;
with
    await b;
    await 10ms;
    x = 0;
with
    var int b = 1;
    var int a = b;
    x = a;
end;
escape x;
]],
    _ana = {
        abrt = 5,
        acc = 1,
        --unreachs = 4,
    },
    run = 1,
}

Test { [[
input int A,B;
par do
    par/or do
        await A;
    with
        await B;
    end;
    escape 1;
with
    await A;
    escape 2;
end;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
}

Test { [[
input int A,B;
par do
    par/and do
        await A;
    with
        await B;
    end;
    escape 1;
with
    await A;
    escape 2;
end;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}

Test { [[
input int A,B, Z;
par do
    loop do
        par/or do
            await A;    // 5
            break;      // 6
        with
            await Z;
        with
            await B;
            break;
        end;
        await Z;
    end;
    escape 1;           // 15
with
    await A;            // 17
    escape 2;           // 18
end;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
}

Test { [[
input int A,B;
var int a = 0;
par/or do
    par/or do
        await A;
    with
        await B;
    end;
    await 10ms;
    var int v = a;
    if v then end;
with
    await B;
    await 10ms;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
    run = {
        ['0~>A ; ~>1ms ; 0~>B ; ~>20ms'] = 0,
    }
}

Test { [[
input int A,B;
var int a = 0;
par/or do
    await A;
    await B;
    await 10ms;
    var int v = a;
with
    await B;
    await 10ms;
    a = 1;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
}

Test { [[
input int A,B;
var int a = 0;
par/or do
    par/and do
        await A;
    with
        await B;
        await 10ms;
    end;
    var int v = a;
    if v then end;
with
    await B;
    await 20ms;
    a = 1;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
    run = {
        ['1~>A;~>10ms;1~>B;~>25ms'] = 0,
        ['~>10ms;1~>B;~>25ms'] = 1,
    }
}

Test { [[
input int A,B;
var int a = 0;
par/or do
    par/or do
        await A;
    with
        await B;
        await (10)us;
    end;
    await 10us;
    var int v = a;
    if v then end
with
    await A;
    await B;
    await (20)us;
    a = 1;
end;
escape a;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
    run = {
        ['0~>A ; 0~>B ; ~>21us'] = 0,
    }
}
Test { [[
input int A,B;
var int a=0;
par/or do
    par/and do
        await A;
    with
        await B;
        await 10ms;
    end;
    await 10ms;
    var int v = a;
with
    await A;
    await B;
    await 20ms;
    a = 1;
end;
escape a;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
}

Test { [[
var int a=0;
par/or do
    await 10ms;
    var int v = a;
with
    await 10ms;
    a = 1;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
}

Test { [[
input int A;
var int v=0;
par do
    loop do
        await A;
        await A;
        v = 1;
    end;
with
    loop do
        await A;
        await A;
        await A;
        v = 2;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,
    },
}

Test { [[
input int A,B;
var int v=0;
par do
    loop do
        await A;
        v = 1;
    end;
with
    loop do
        await A;
        await B;
        await A;
        v = 2;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,       -- fiz na mao!
    },
}
-- bom exemplo de explosao de estados!!!
Test { [[
input int A,B;
var int v=0;
par do
    loop do
        await A;
        await A;
        v = 1;
    end;
with
    loop do
        await A;
        await B;
        await A;
        await A;
        v = 2;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,       -- nao fiz na mao!!!
    },
}

-- EX.04: join
Test { [[
input int A,B;
var int a=0;
par/and do
    par/or do
        await A;
    with
        await B;
    end;
    a = 1;
with
    await A;
    await B;
end;
escape a;
]],
    run = {
        ['0~>A ; 0~>B'] = 1,
        ['0~>B ; 0 ~>A ; 0~>B'] = 1,
    }
}

Test { [[
input int A;
var int a=0;
par/and do
    if a then
        await A;
    else
        await A;
        await A;
        var int v = a;
    end;
with
    await A;
    a = await A;
with
    await A;
    await A;
    a = await A;
end;
escape a;
]],
    _ana = {
        --acc = 1,
        acc = 3,
    },
}
Test { [[
input int A;
var int a=0;
if a then
    await A;
else
    par/and do
        await A;
        await A;
        var int v = a;
    with
        await A;
        a = await A;
    with
        await A;
        await A;
        a = await A;
    end;
end;
escape a;
]],
    _ana = {
        --acc = 1,
        acc = 3,
    },
}
Test { [[
input int A;
var int a=0;
par do
    loop do
        if a then           // 5
            await A;
        else
            await A;
            await A;
            var int v = a;  // 10
        end;
    end;
with
    loop do
        await A;
        a = await A;        // 16
    end;
with
    loop do
        await A;
        await A;
        a = await A;        // 22
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 5,
    },
}
Test { [[
var int v = do/_ par do
            escape 0;
        with
            escape 0;
end
        end;
if v then
    escape 1;
else
    if 1==1 then
        escape 1;
    else
        escape 0;
    end;
end;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
var int a=0;
var int v = do/_ par do
            escape 0;
        with
            escape 0;
end
        end;
if v then
    a = 1;
else
    a = 1;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
var int v = do/_ par do
            escape 1;
        with
            escape 2;
end
        end;
escape v;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}

Test { [[
var int a=0;
par/or do
    loop do
        await 10ms;
        a = 1;
    end;
with
    await 100ms;
    a = 2;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 1,
    },
}
Test { [[
var int a=0;
par/or do
    await (10)us;
    a = 1;
with
    await (5)us;
    await (10)us;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
}
Test { [[
input int A;
var int a=0;
par/or do
    await (10)us;
    await A;
    a = 1;
with
    await (5)us;
    await A;
    await (10)us;
    await A;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
}
Test { [[
input int A;
var int a=0;
par/or do
    await (10)us;
    await A;
    a = 1;
with
    await (5)us;
    await A;
    await A;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
}
Test { [[
input int A;
var int a=0;
par/or do
    await 10ms;
    await A;
    a = 1;
with
    await (5)us;
    await A;
    await A;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
}

Test { [[
var int a=0;
par/or do
    await (10)us;
    await 10ms;
    a = 1;
with
    await 10ms;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
}

Test { [[
par/or do
    loop do
        break;  // 3
    end
with
    nothing;    // 6
end
escape 0;
]],
    _ana = {
        abrt = 4,   -- TODO: break is inside par/or (should be 3)
    },
    run = 0,
}

Test { [[
var int a=0;
par/or do
    loop do
        await 10ms;     // 4
        if (true) then
            break;      // 6
        end;
    end;
    a = 1;
with
    await 100ms;        // 11
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 4,   -- TODO: break is inside par/or (should be 3)
        acc = 1,
    },
}

Test { [[
var int a=0;
par/or do
    loop do
        await 11ms;
        if (true) then
            break;
        end;
    end;
    a = 1;
with
    await 100ms;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 4,
        acc = 1,
    },
}

Test { [[
input int A;
var int a=0;
par/or do
    await 10ms;
    await A;
    a = 1;
with
    await (10)us;
    await A;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
}

Test { [[
input int A;
var int a=0;
par/or do
    await (10)us;
    await A;
    a = 1;
with
    await (10)us;
    await A;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
}

Test { [[
input int A;
var int a=0;
par/or do
    await A;
    await 10ms;
    a = 1;
with
    await 10ms;
    a = 2;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        --unreachs = 1,
        acc = 1,
    },
    run = {
        ['~>10ms'] = 2,
        ['~>1ms ; 1~>A ; ~>10ms'] = 2,
    }
}

Test { [[
var int a=0;
await (10)us;
par/or do
    await 10ms;
    a = 2;
with
    await 20ms;
    a = 3;
end
escape a;
]],
    --todo = 'wclk_any=0',
    _ana = {
        acc = 1,
        abrt = 3,
        unreachs = 1,
    },
    run = { ['~>1s']=2 },
}
Test { [[
input int A;
var int a=0;
par/or do
    await (10)us;
    a = 1;
    par/or do
        await 10ms;
        a = 2;
        await A;
    with
        await 20ms;
        a = 3;
        await A;
    end;
with
    await 30ms;
    a = 4;
end;
escape a;
]],
    todo = 'wclk_any=0',
    _ana = {
        acc = 3,
    },
}

Test { [[
input void A;
var int a=0;
par/or do
    await A;
    await (10)us;
    a = 1;
with
    await (10)us;
    a = 2;
end;
escape a;
]],
    _ana = {
        --unreachs = 1,
        acc = 1,
        abrt = 3,
    },
    run = {
        ['~>10ms'] = 2,
        ['~>1ms; ~>A ; ~>10ms'] = 2,
    }
}

Test { [[
var int x=0;
par/or do
    await 12ms;
    x = 0;
with
    await 4ms;
    await 4ms;
    await 4ms;
    x = 1;
end;
escape x;
]],
    _ana = {
        abrt = 5,
        acc = 1,
    },
}

Test { [[
var int x=0;
par do
    loop do
        await 10ms;
        x = 1;
    end;
with
    loop do
        await (200)us;
        x = 2;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,
    },
}

Test { [[
var int x=0;
par do
    loop do
        x = await 10ms;
    end;
with
    loop do
        x = await 200ms;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,
    },
}

Test { [[
event void a;
var int x=0;
par/or do
    par/and do
        await 10ms;
        x = 4;
    with
        emit a;
    end;
    var int v = x;
with
    await a;
    await 10ms;
    x = 5;
end;
escape x;
]],
    _ana = {
        abrt = 3,
        acc = 3,
    },
    --run = { ['~>15ms']=5, ['~>25ms']=5 }
}

-- EX.02: trig e await depois de loop
Test { [[
input int A;
event int a;
loop do
    par/and do
        await A;
        emit a => 1;
    with
        await a;
    end;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
event void a;
par/or do
    emit a;
with
    await a;
end
escape 0;
]],
    _ana = {
        --unreachs = 1,
        acc = 1,
        abrt = 1,
    },
    run = 0,
}

Test { [[
input void A;
event void a;
var int ret = 0;
par/or do
    await A;
    emit a;
with
    await a;
    ret = 1;
end
ret = ret + 1;
escape ret;
]],
    _ana = {
        --unreachs = 1,
    },
    run = { ['~>A']=2 },
}

Test { [[
event void a;
loop do
    par/and do
        emit a;
    with
        await a;
    end
end
]],
    todo = 'TIGHT',
    _ana = {
        loop = true,
    },
}

Test { [[
event int a;
par do
    loop do
        par/or do
            emit a => 1;
        with
            await a;
        end;
    end;
with
    var int aa = await a;
    emit a => aa;
end;
]],
    _ana = {
        abrt = 1,
        acc = 2,
        isForever = true,
    },
    loop = 'line 3 : tight loop',
}

Test { [[
input int A;
event int a, d, e, i, j;
var int dd=0, ee=0;
par/and do
    await A;
    emit a => 1;
with
    dd = await a;
    emit i => 5;
with
    ee = await a;
    emit j => 6;
end;
escape dd + ee;
]],
    --trig_wo = 2,
    run = {
        ['0~>A'] = 2,
    }
}

Test { [[
event int a;
var int aa=0;
par do
    emit a => 1;
    aa = 1;
with
    escape aa;
end;
]],
    _ana = {
        --nd_esc = 1,
        abrt = 1,
        --unreachs = 1,
        --trig_wo = 1,
        acc = 1,
    },
}

Test { [[
event int a;
var int v=0,aa=1;
loop do
    par do
        v = aa;
    with
        await a;
    end;
end;
]],
    _ana = {
        isForever = true,
        unreachs = 1,
        reachs = 1,
    },
}
Test { [[
input int A;
event int b;
var int a=1,v=0;
par do
    loop do
        v = a;
        await b;
    end;
with
    await A;
    emit b => 1;
end;
]],
    _ana = {
        isForever = true,
    },
}
Test { [[
input int A,B;
event int a;
par do
    par do
        await A;
        emit a => 1;
    with
        await a;
        await a;
    end;
with
    var int v = await B;
    escape v;
end;
]],
    _ana = {
        --unreachs = 1,
        reachs = 1,
    },
    run = {
        ['0~>A ; 10~>B'] = 10,
    }
}

Test { [[
input void OS_START;
event int a;
var int b=0;
par/or do
    b = await a;
with
    await OS_START;
    emit a => 3;
end;
escape b+b;
]],
    _ana = {
        --unreachs = 1,
        --trig_wo = 1,
    },
    run = 6,
}

Test { [[
event int a;
var int b=0;
par/or do
    b = await a;        // 4
with
    emit a => 3;           // 6
with
    var int a = b;
end;
escape 0;
]],
    _ana = {
        abrt = 5,
        --unreachs = 2,
        acc = 1,
        --trig_wo = 1,
    },
}

Test { [[
input void OS_START;
event int b;
var int i=0;
par/or do
    await OS_START;
    emit b => 1;
    i = 2;
with
    await b;
    i = 1;
end;
escape i;
]],
    _ana = {
        --nd_esc = 1,
        unreachs = 0,
    },
    run = 1,
}
Test { [[
input void OS_START;
event int b,c;
var int cc=0;
par/or do
    await OS_START;
    emit b => 1;
    cc = await c;
with
    await b;
    cc = 5;
    emit c => 5;
end;
escape cc;
]],
    _ana = {
        --nd_esc = 1,
        unreachs = 0,
        --trig_wo = 1,
    },
    run = 5,
}
-- TODO: ret=0 should not be required because the loop cannot escape w/o assigning to v
Test { [[
input int A;
var int ret=0;
loop do
    var int v = await A;
    if v == 5 then
        ret = 10;
        break;
    else
    end;
end;
escape ret;
]],
    run = {
        ['1~>A ; 2~>A ; 3~>A ; 4~>A ; 5~>A'] = 10,
        ['5~>A'] = 10,
    }
}
Test { [[
input int B;
var int a = 0;
loop do
    var int b = await B;
    a = a + b;
    if a == 5 then
        escape 10;
    end;
end;
escape 0;   // TODO
]],
    _ana = {
        unreachs = 1,
    },
    run = {
        ['1~>B ; 4~>B'] = 10,
        ['3~>B ; 2~>B'] = 10,
    }
}

Test { [[
input int A,B;
var int ret = do/_ loop do
        await A;
        par/or do
            await A;
        with
            var int v = await B;
            escape v;
        end;
    end end;
escape ret;
]],
    run = {
        ['1~>A ; 5~>B'] = 5,
        ['1~>A ; 1~>A ; 3~>B ; 1~>A ; 5~>B'] = 5,
    }
}

Test { [[
input int A,B;
var int ret = loop do
        await A;
        par/or do
            await A;
        with
            var int v = await B;
            escape v;
        end;
    end;
escape ret;
]],
    parser = 'line 2 : after `=´ : expected expression',
    run = {
        ['1~>A ; 5~>B'] = 5,
        ['1~>A ; 1~>A ; 3~>B ; 1~>A ; 5~>B'] = 5,
    }
}

Test { [[
input int A;
event int a;
var int aa=0;
loop do
    var int v = await A;
    if v==2 then
        escape aa;
    end;
    emit a => v;
    aa = v;
end;
]],
    _ana = {
        --trig_wo = 1,
    },
    run = {
        ['0~>A ; 0~>A ; 3~>A ; 2~>A'] = 3,
    }
}

Test { [[
input int A;
event int a;
var int aa=0;
loop do
    var int v = await A;
    if v==2 then
        escape aa;
    else
        if v==4 then
            break;
        end;
    end;
    emit a => v;
    aa = v;
end;
escape aa-1;
]],
    --trig_wo = 1,
    run = {
        ['0~>A ; 0~>A ; 3~>A ; 2~>A'] = 3,
        ['0~>A ; 0~>A ; 3~>A ; 4~>A'] = 2,
    }
}

Test { [[
input int A,B;
var int a = 0;
par do
    await B;
    escape a;
with
    loop do
        await A;
        a = a+1;
        await A;
        a = a+1;
    end;
end;
]],
    run = {
        ['0~>A ; 0~>A ; 0~>B'] = 2,
    }
}


Test { [[
input int A,B;
loop do
    par/or do
        await A;
        break;
    with
        await B;
    end;
end;
escape 1;
]],
    run = {
        ['2~>A ; 4~>A'] = 1,
        ['0~>B ; 0~>B ; 2~>A ; 0~>B'] = 1,
    }
}

    -- UNREACH

Test { [[
input int A,B;
var int ret = 0;
par/or do await A; with await B; end;
par/or do
    ret=await A;
with
    ret=await B;
end;
escape ret;
]],
    run = {
        ['1~>A;2~>B;1~>A'] = 2,
        ['2~>B;1~>A;2~>B'] = 1,
    },
}

Test { [[
input int A,B;
var int ret=0;
par/or do
    par/or do
        await A;
    with
        await B;
    end;
    par/or do
        await A;
    with
        await B;
    end;
    par/or do
        ret=await A;
    with
        ret=await B;
    end;
with
    await B;
    await B;
    await B;
    await B;
end;
escape ret;
]],
    _ana = {
        abrt = 8,   -- TODO: not checked
        --unreachs = 1,
    },
    run = {
        ['0~>A ; 0~>A ; 10~>A'] = 10,
        ['0~>B ; 0~>A ; 11~>B'] = 11,
        ['0~>B ; 0~>B ; 12~>B'] = 12,
    }
}

Test { [[
input int A,B;
var int v=0;
par/or do
    v = await A;
with
    await B;
    v = await A;
end;
escape v;
]],
    _ana = {
        acc = 1,     -- should be 0
        abrt = 3,
    },
    run = {
        ['10~>A'] = 10,
        ['0~>B ; 10~>A'] = 10,
    }
}

Test { [[
input int A,B,Z;
var int ret=0;
par/or do
    par/or do
        ret = await A;
    with
        ret = await B;
    end;
with
    await Z;
    ret = await A;
end;
escape ret;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['0~>Z ; 10~>B'] = 10,
        ['0~>Z ; 10~>A'] = 10,
        ['0~>Z ; 1~>Z ; 5~>A'] = 5,
    }
}

Test { [[
input int A,B,Z;
var int v=0;
par/or do
    par/and do
        v = await A;
    with
        await B;
    end;
with
    await Z;
    v = await A;
end;
escape v;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = {
        ['0~>Z ; 10~>A'] = 10,
        ['0~>A ; 1~>Z ; 5~>A ; 1~>B'] = 5,
    }
}

Test { [[
input int A,B,Z;
var int v=0;
par/or do
    par/and do
        await A;
        await B;
        v = await A;
    with
        await Z;
        await B;
        await Z;
    end;
with
    await A;
    await Z;
    await B;
    await B;
    v = await Z;
end;
escape v;
]],
    _ana = {
        abrt = 6,   -- TODO: not checked
    },
    run = {
        ['0~>A ; 1~>Z ; 5~>B ; 1~>B ; 9~>Z'] = 9,
        ['0~>A ; 1~>Z ; 1~>B ; 5~>A ; 9~>Z'] = 5,
    }
}

Test { [[
input int A,B,Z;
var int v=0;
par/or do
    par/and do
        await A;
        await B;
        v = await A;
    with
        await Z;
        await B;
        v = await Z;
    end;
with
    await A;
    await Z;
    await B;
    await A;
    await Z;
    await B;
end;
escape v;
]],
    _ana = {
        abrt = 6,   -- TODO: not checked
        --unreachs = 1,
    },
    run = {
        ['0~>A ; 1~>Z ; 5~>B ; 1~>A ; 1~>Z ; 9~>B'] = 1,
    },
}

Test { [[
input int A,B;
var int v=0;
par/or do
    if true then
        v = await A;
    else
        v = await B;
    end;
with
    await A;
    v = 1;
with
    await B;
    v = 2;
end;
escape v;
]],
    _ana = {
        acc = 2,
        abrt = 6,
    },
    run = {
        ['1~>A'] = 1,
        ['1~>B'] = 2,
    },
}
Test { [[
input int A,B;
var int v=0;
par/or do
    if true then
        v = await A;
    else
        v = await B;
    end;
with
    await A;
    v = await B;
with
    await B;
    v = await A;
end;
escape v;
]],
    _ana = {
        acc = 2,
        abrt = 10,
    },
    run = {
        ['0~>B ; 10~>A'] = 10,
        ['0~>B ; 9~>A'] = 9,
    },
}
Test { [[
input int A,B,Z;
var int v=0;
par/or do
    await A;
    await B;
    v = await Z;
with
    await B;
    await A;
    v = await Z;
end;
escape v;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
input int A,B,Z;
var int v=0;
par/or do
    if true then
        v = await A;
    else
        v = await B;
    end;
with
    await A;
    await B;
    v = await Z;
with
    await B;
    await A;
    v = await Z;
end;
escape v;
]],
    _ana = {
        --unreachs = 2,
        acc = 1,
        abrt = 7,   -- TODO: not checked
    },
    run = {
        ['0~>B ; 10~>A'] = 10,
    },
}
Test { [[
input int A,B;
par do
    loop do
        await A;
        var int v = await B;
        escape v;
    end;
with
    var int v = await A;
    escape v;
end;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = {
        ['0~>B ; 10~>A'] = 10,
    },
}
Test { [[
input int A,B;
var int v=0;
loop do
    par/or do
        await A;
    with
        v = await B;
        break;
    end;
end;
escape v;
]],
    run = {
        ['0~>A ; 0~>A ; 10~>B'] = 10,
    },
}
Test { [[
input int A,B,Z;
var int a = 0;
par do
    loop do
        if a then
            a = await A;
            break;
        else
            await B;
        end;
    end;
    escape a;
with
    await B;
    a = await Z;
    escape a;
end;
]],
    run = {
        ['0~>B ; 10~>Z'] = 10,
    },
}

Test { [[
input int A,B;
if true then
    var int v = await A;
    escape v;
else
    var int v = await B;
    escape v;
end;
]],
--(11?~A:~B)]],
    run = {
        ['10~>A ; 10~>B'] = 10,
        ['0~>B ; 9~>A'] = 9,
    },
}
Test { [[
input int A,B;
loop do
    await A;
end;
if true then       // TODO: unreach
    await A;
else
    await B;
end;
escape 1;
]],
    _ana = {
        isForever = true,
        unreachs = 1,
    },
}
Test { [[
par/or do
with
end
escape 1;
]],
    _ana = {
        abrt = 3,
    },
    run = 1,
}
Test { [[
input int A;
par do
with
    loop do
        await A;
    end;
end;
]],
--1and(~A)*]],
    _ana = {
        isForever = true
    },
}
Test { [[
input int A,B;
par do
    loop do
        await A;
    end;
with
    loop do
        await B;
    end;
end;
]],
--(~A)* and (~B)*]],
    _ana = {
        isForever = true,
    },
}
Test { [[
input int A,B,Z;
var int v=0;
loop do
    v = await A;
    if v then
        v = await B;
        break;
    else
        await Z;
    end
end
escape v;
]],
--(((~A)?~B^:~Z))*]],
    run = {
        ['1~>A ; 10~>B'] = 10,
        ['0~>A ; 0~>Z ; 1~>A ; 9~>B'] = 9,
    },
}

Test { [[
input int A,B,Z,X,E,EE,GG,H,I,J,K,L;
var int v=0;
par/or do
    await A;
with
    await B;
end;
await Z;
await X;
await E;
await EE;
var int g = await GG;
if g then
    await H;
else
    await I;
end;
await J;
loop do
    par/or do
        await K;
    with
        v = await L;
        break;
    end;
end;
escape v;
]],
    run = {
        ['0~>A ; 0~>Z ; 0~>X ; 0~>E ; 0~>EE ; 0~>GG ; 0~>I ; 0~>J ; 0~>K ; 10~>L'] = 10,
        ['0~>B ; 0~>Z ; 0~>X ; 0~>E ; 0~>EE ; 1~>GG ; 0~>H ; 0~>J ; 0~>K ; 11~>L'] = 11,
    },
}

-- NONDET

Test { [[
var int a=0;
par do
    a = 1;
    escape 1;
with
    escape a;
end;
]],
    _ana = {
        abrt = 3,
    acc = 2,
    },
}
Test { [[
input int B;
var int a=0;
par do
    await B;
    a = 1;
    escape 1;
with
    await B;
    escape a;
end;
]],
    _ana = {
        acc = 2,
        abrt = 3,
    },
}
Test { [[
input int B,Z;
event int a;
var int aa=0;
par do
    await B;
    aa = 1;
    escape 1;
with
    par/or do
        await a;
    with
        await B;
    with
        await Z;
    end;
    escape aa;
end;
]],
    _ana = {
        --unreachs = 1,
        abrt = 3,
        acc = 2,
    },
}
Test { [[
input int Z;
event int a;
var int aa=0;
par do
    emit a => 1;       // 5
    aa = 1;
    escape 10;
with
    par/or do
        await a;    // 10
    with
    with
        await Z;
    end;
    escape aa;
end;
]],
    _ana = {
        acc = 1,
        abrt = 3,
        --unreachs = 2,    -- +1 native unreachs
    },
    --run = 1,
    run = 10,
}
Test { [[
input void OS_START;
event int a;
par do
    await OS_START;
    emit a => 1;
with
    var int aa = await a;
    escape aa;
end;
]],
    _ana = {
        --unreachs = 1,
        --nd_esc = 1,
    },
    run = 1,
}
Test { [[
input int B,Z;
event int a;
var int aa=0;
par/or do
    await B;
    emit a => 5;
with
    aa = await a;
    aa = aa + 1;
end;
escape aa;
]],
    _ana = {
        --unreachs = 1,
        --nd_esc = 1,
    },
    run = {
        ['1~>B'] = 6,
    },
}
Test { [[
input int B,Z;
event int a;
var int aa=0;
par/or do
    await B;
    emit a => 5;
with
    par/and do
        aa = await a;   // 9
    with
        aa = await a;   // 11
    end
    aa = aa + 1;
end;
escape aa;
]],
    _ana = {
        --unreachs = 1,
        --nd_esc = 1,
        acc = 1,
    },
    run = {
        ['1~>B'] = 6,
    },
}
Test { [[
input int B;
event int a;
var int aa=0;
par/or do
    await B;        // 5
    emit a => 5;
    aa = 5;
with
    par/and do      // 9
        await B;    // 10
    with
        await a;
    end
    aa = aa + 1;
end;
escape aa;
]],
    _ana = {
        acc = 2,
        --unreachs = 1,
        abrt = 3,
    },
    run = {
        --['1~>B'] = 6,
        ['1~>B'] = 5,
    },
    --todo = 'nd excpt',
}
Test { [[
input int B,Z;
event int a;
var int aa=5;
par/or do
    await B;
    emit a => 5;
with
    par/and do
        aa = await a;
    with
        await B;
    with
        await Z;
    end;
    aa = aa + 1;
end;
escape aa;
]],
    _ana = {
        abrt = 3,
    },
    run = {
        ['1~>B'] = 5,
        ['2~>Z; 1~>B'] = 5,
    },
}
Test { [[
event int a;
var int aa = 1;
par do
    emit a => 0;
    escape aa;  // 5
with
    par/and do  // 7
        aa = await a;
    with
    end;
    escape aa;
end;
]],
    _ana = {
        acc = 1,
        abrt = 1,
        --unreachs = 1,
    },
    run = 1,
}
Test { [[
input int Z;
event int a;
var int aa = 0;
par do
    emit a => 1;
    aa = 1;
    escape aa;
with
    par/and do
        aa = await a;
    with
    with
        await Z;
    end;
    escape aa;
end
]],
    _ana = {
        acc = 1,
        --nd_esc = 1,
        abrt = 1,
    },
    run = 1,
}
Test { [[
input int Z;
event int a;
var int aa = 0;
par do
    emit a => 1;
    aa = 1;
    escape aa;
with
    par/and do
        aa = await a;
    with
    with
        await Z;
    end;
    escape aa;
end
]],
    safety = 2,
    _ana = {
        acc = 5,
        --nd_esc = 1,
        abrt = 1,
    },
    run = 1,
}
Test { [[
input int B,Z;
event int a;
var int aa=0;
par do
    await B;
    aa = 1;
    escape aa;
with
    par do
        await a;
    with
        await B;
    with
        await Z;
    end;
end;
]],
    _ana = {
        --unreachs = 1,
        abrt = 1,
        reachs = 1,
    },
    run = {
        ['1~>B'] = 1,
    },
}
Test { [[
input int B,Z;
event int aa;
var int a=0;
par do
    await B;
    a = 1;
    escape a;
with
    par/and do
        await aa;
    with
        await B;
    with
        await Z;
    end;
    escape a;
end;
]],
    _ana = {
        abrt = 3,
        --unreachs = 2,
        acc = 2,
    },
    run = {
        ['1~>B'] = 1,
    },
}
Test { [[
input int A;
loop do
    par/or do
        break;
    with
        par/or do
        with
        end;
        await A;
        // unreachable
    end;
    // unreachable
end;
escape 1;
]],
    _ana = {
        abrt = 4,
        --unreachs = 3,
    },
    run = 1,
}

Test { [[
input int A;
loop do
    par/or do
        break;
    with
        break;
    end
end
escape 1;
]],
    _ana = {
        abrt = 3,
        unreachs = 2,
    },
    run = 1,
}

Test { [[
input int A;
par/or do
    escape 1;
with
    await A;
end;
]],
    _ana = {
        --unreachs = 2,
        abrt = 1,
        reachs = 1,
    },
    run = 1,
}
Test { [[
input int A;
loop do
    par/or do
        break;
    with
        par/or do
            escape 1;
        with
            await A;
            // unreachable
        end;
        // unreachable
        await A;
        // unreachable
    end;
    // unreachable
end;
escape 2;       // executes last
]],
    _ana = {
        --unreachs = 5,
        abrt = 4,
    },
    run = 2,
}

Test { [[
input int A;
loop do
    par do
        break;
    with
        escape 1;
    with
        await A;
    end;
end;
escape 2;   // executes last
]],
    _ana = {
        unreachs = 1,
        abrt = 5,
    },
    run = 2,
}

Test { [[
input int A;
loop do
    par do
        break;          // 4
    with
        par do
            escape 1;   // 7
        with
            await A;    // 9
            // unreachable
        end;
    end;
end;
escape 2;   // executes last
]],
    _ana = {
        unreachs = 1,
        abrt = 4,
    },
    run = 2,
}

Test { [[
input void A;
loop do
    await A;
    par/or do
        break;          // prio1
    with
        par/or do
        with
        end;
                        // prio2
    end;
end;
escape 1;
]],
    _ana = {
        abrt = 6,       -- TODO: not checked
    },
    run = { ['~>A'] = 1, },
}
Test { [[
par/or do
with
    par/or do
    with
    end;
end;
escape 1;
]],
    _ana = {
        abrt = 6,
    },
    run = 1,
}
Test { [[
event int a;
var int aa=0;
par/or do
    aa = 1;
with
    par/or do
        await a;
    with
    end;
    escape aa;
end;
escape aa;
]],
    _ana = {
        --unreachs = 1,
        abrt = 4,
        acc  = 1,
    },
}
Test { [[
input int B;
event int a;
var int aa=0;
par do
    await B;
    aa = 1;
    escape aa;
with
    await B;
    par/or do
        await a;
    with
    end;
    escape aa;
end;
]],
    _ana = {
        --unreachs = 1,
        abrt = 4,
        acc = 2,
    },
}
Test { [[
var int a = 0;
par do
    escape a;
with
    escape a;
end;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
var int a;
par do
    escape a;
with
    a = 1;
    escape a;
end;
]],
    ref = 'line 1 : uninitialized variable "a" crossing compound statement (tests.lua:2)',
}
Test { [[
var int a=0;
par do
    escape a;
with
    a = 1;
    escape a;
end;
]],
    _ana = {
        acc = 2,
        abrt = 3,
    },
}
Test { [[
var int a=0;
par do
    a = 1;
    escape a;
with
    escape a;
end;
]],
    _ana = {
        abrt = 3,
        acc = 2,
    },
}
Test { [[
var int a=0;
par/or do
    a = 1;
with
    a = 1;
end;
escape a;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
}
Test { [[
var int a=0;
par/or do
    a = 1;
with
    a = 1;
with
    a = 1;
end;
escape a;
]],
    _ana = {
        abrt = 9,
        acc = 3,
    },
}
Test { [[
input int A;
par do
    var int v = await A;
    escape v;
with
    var int v = await A;
    escape v;
end;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}

-- TODO: STACK
Test { [[
event int a;
var int aa=10;
par/or do
    await a;
with
    emit a => 1;
    aa = 1;
end;
escape aa;
]],
    _ana = {
        abrt = 1,
        --unreachs = 1,
        acc = 1,
        --trig_wo = 1,
    },
    run = 1,
    --run = 10,
}
Test { [[
event int a;
par/or do
    emit a => 1;
with
    emit a => 1;
end;
escape 1;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
    run = 1,
}
Test { [[
event int a,b;
var int aa=2,bb=2;
par/or do
    emit a => 1;
    aa = 2;
with
    emit b => 1;
    bb = 5;
end;
escape aa+bb;
]],
    _ana = {
        abrt = 3,
    },
    --run = 7,
    run = 4,
}
Test { [[
var int a=0, b=0;
par/or do
    a=2;
with
    b=3;
end;
escape a+b;
]],
    _ana = {
        abrt = 3,
    },
    --trig_wo = 2,
    --run = 5,
    run = 2,
}
Test { [[
event int a;
var int aa=0;
var int v = do/_ par do
    emit a => 1;
    aa = 1;
    escape aa;
with
    emit a => 1;
    escape aa;
with
    emit a => 1;
    escape aa;
end
end;
escape v;
]],
    _ana = {
        acc = 8, -- TODO: not checked
        abrt = 9,
        --trig_wo = 3,
    },
}
Test { [[
var int v;
v = do/_ par do
    escape 1;
with
    escape 1;
with
    escape 1;
end
end;
escape v;
]],
    _ana = {
        acc = 3,
        abrt = 9,
    --trig_wo = 1,
    },
}
Test { [[
input int A;
var int a = 0;
par do
    await A;
    escape a;
with
    await A;
    escape a;
end;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
}
Test { [[
input int A;
var int a=0;
par do
    await A;
    escape a;
with
    await A;
    a = 1;
    escape a;
end;
]],
    _ana = {
        abrt = 3,
        acc = 2,
    },
}
Test { [[
input int A;
event int a;
await A;
emit a => 1;
await A;
emit a => 1;
escape 1;
]],
--~A;1~>a;~A;1~>a]],
    --trig_wo = 2,
    run = {
        ['0~>A ; 10~>A'] = 1,
    },
}
Test { [[
input void OS_START;
input int A;
event int a;
var int ret=0;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end;
with
    await A;
    await A;
    ret = await a;
end;
escape ret;
]],
    _ana = {
        acc = 1,
        --nd_esc = 1,
    },
    run = { ['1~>A;2~>A;3~>A']=3 },
}
Test { [[
input void OS_START;
event int a;
par do
    await OS_START;
    emit a => 1;
    escape 1;
with
    var int aa = await a;
    aa = aa + 1;
    escape aa;      // 10
with
    await a;        // 12
    await FOREVER;
end;
]],
    _ana = {
        --nd_esc = 1,
        --unreachs = 1,
        abrt = 1,
    },
    run = 2,
}
Test { [[
event int a;
var int aa=0;
par/or do
    emit a => 1;
    aa = 1;
with
    aa = await a;
    aa = aa + 1;
with
    var int aa = await a;
    var int v = aa;
end;
escape aa;
]],
    _ana = {
        --nd_esc = 1,
        --unreachs = 1,
        acc = 2,
        abrt = 2,
    },
}
Test { [[
event int a;
var int aa=0;
par/or do
    emit a => 1;
    aa = 1;
with
    aa = await a;
    aa = aa + 1;
with
    var int aa = await a;
    var int v = aa;
end;
escape aa;
]],
    safety = 2,
    _ana = {
        --nd_esc = 1,
        --unreachs = 1,
        acc = 5,
        abrt = 2,
    },
}
Test { [[
input int A;
var int v=0;
par do
    await A;
    loop do
        await A;
        var int a = v;
    end;
with
    loop do
        await A;
        await A;
        v = 2;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,
    },
}
Test { [[
input int A;
var int v=0;
par do
    loop do
        await A;
        await A;
        v = 1;
    end;
with
    loop do
        await A;
        await A;
        await A;
        v = 2;
    end;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,
    },
}
Test { [[
input int A, B;
var int a=0;
par/or do
    var int v = await A;
    a = v;
with
    var int v = await B;
    a = v;
with
    await A;
    await B;
    var int v = a;
    if v then end;
end;
escape a;
]],
    _ana = {
        --unreachs = 1,
        acc = 1,
        abrt = 4,
    },
    run = {
        ['3~>A'] = 3,
        ['1~>B'] = 1,
    },
}
Test { [[
input int A, B;
var int a=0;
par/or do
    await A;
    await B;
    a = 1;
with
    await A;
    var int v = await B;
    a = v;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
}
Test { [[
input int A, B;
var int a=0;
par/or do
    await A;
    a = 3;
with
    await B;
    a = 1;
end;
await B;
escape a;
]],
    run = {
        ['3~>A ; 5~>B'] = 3,
        ['3~>A ; 5~>B ; 5~>B'] = 3,
        ['3~>B ; 5~>B'] = 1,
        ['3~>B ; 5~>A ; 5~>B'] = 1,
    },
}

Test { [[
input int A, B, Z;
var int v=0;
par/or do
    v = await A;
with
    par/or do
        v = await B;
    with
        v = await Z;
    end;
end;
escape v;
]],
    run = {
        ['10~>A ; 1~>A'] = 10,
        ['9~>B'] = 9,
        ['8~>Z'] = 8,
    }
}
Test { [[
var int v = 0;
par/or do
    par/or do
        v = 1;
    with
        v = 2;
    end
    v = 3;
    await FOREVER;
with
    v = 4;
end;
escape v;
]],
    _ana = { acc=true },
    run = 4,
}

Test { [[
input int A;
par/or do
with
end;
var int v = await A;
escape v;
]],
    _ana = {
        abrt = 3,
    },
    run = {
        ['10~>A ; 1~>A'] = 10,
        ['9~>A'] = 9,
        ['8~>A'] = 8,
    }
}
Test { [[
var int a=0, b=0, c=0, d=0;
event int aa, bb, cc, dd;
par/or do
    par/and do
        await aa;
    with
        await bb;
    with
        await cc;
    end;
    await dd;
with
    par/or do
        emit bb => 1;
        b=1;
    with
        emit aa => 2;
        a=2;
    with
        emit cc => 3;
        c=3;
    end;
    emit dd => 4;
    d=4;
end;
escape a+b+c+d;
]],
    _ana = {
        acc = 3,
        abrt = 10,  -- TODO: not checked
        --unreachs = 1,
    },
    --run = 10,
    run = 5,
}
Test { [[
event int a, b, c;
var int aa=0, bb=0, cc=0;
par/or do
    par/or do
        await a;
        aa=10;
    with
        await b;
    with
        await c;
    end;
with
    par/or do
        emit a => 10;
        aa=10;
    with
        emit b => 20;
        bb=20;
    with
        emit c => 30;
        cc=30;
    end;
end;
escape aa+bb+cc;
]],
    _ana = {
        acc = 3,
        abrt = 10,  -- TODO: not checked
        --unreachs = 4,
    },
    --run = 60,
    run = 10,
}
Test { [[
event int a, b, c;
par/or do
    par do
        await a;
    with
        await b;
    with
        await c;
    end;
with
    par/or do
        emit a => 10;
    with
        emit b => 20;
    with
        emit c => 30;
    end;
end;
escape 10;
]],
    _ana = {
        acc = 3,
        abrt = 10,  -- TODO: not checked
        reachs = 1,
        --trig_wo = 3,
    },
    --run = 60,
    run = 10,
}
Test { [[
event int a;
par/or do
    emit a => 1;
with
    emit a => 1;
    await a;
end;
escape 0;
]],
    _ana = {
        acc = 2,
        abrt = 1,
        --unreachs = 1,
        --trig_wo = 2,
    },
}
Test { [[
event int a;
par/or do
    emit a => 1;
    await a;
with
    emit a => 1;
end;
escape 0;
]],
    _ana = {
        acc = 2,
        abrt = 1,
        --unreachs = 1,
        --trig_wo = 2,
    },
}
Test { [[
event int a;
par do
    emit a => 1;
with
    emit a => 1;
    await a;
end;
]],
    _ana = {
        isForever = true,
        acc = 2,
        --unreachs = 1,
        reachs = 1,
        --trig_wo = 2,
    },
}
Test { [[
input int B;
loop do
    par do
        break;
    with
        await B;
    end;
end;
escape 1;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = 1,
}
Test { [[
input int A, B;
var int v=0;
loop do
    par/or do
        v = await A;
        break;
    with
        await B;
    end;
end;
escape v;
]],
    run = {
        ['4~>A'] = 4,
        ['1~>B ; 3~>A'] = 3,
    }
}
Test { [[
input int A;
par do
with
    loop do
        await A;
    end;
end;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
var int x=0;
par/or do
    await 8ms;
    x = 0;
with
    await 4ms;
    await 4ms;
    x = 1;
end;
escape x;
]],
    _ana = {
        acc = 1,
        abrt = 4,
    },
}

    -- PRIO

-- prios
Test { [[
var int ret = 10;
loop do
    par/or do
    with
        break;
    end
    ret = 5;
    escape ret;
end
escape ret;
]],
    _ana = {
        unreachs = 1,
        abrt = 3,
    },
    run = 5,
}

Test { [[
var int ret = 10;
loop do
    par/or do
        escape 100;
    with
        break;
    end
    ret = 5;
    await 1s;
end
escape ret;
]],
    _ana = {
        abrt = 3,
        unreachs = 3,
    },
    --run = 10,
    run = 100,
}

Test { [[
var int a = 0;
par/or do
    par/or do
    with
    end;
    a = a + 1;
with
end;
a = a + 1;
escape a;
]],
    _ana = {
        abrt = 6,
    },
    run = 2,
}

Test { [[
var int b=0;
par do
    escape 3;
with
    b = 1;
    escape b+2;
end;
]],
    _ana = {
        abrt = 3,
        acc = 1,
    },
}

Test { [[
input int A;
var int v=0;
loop do
    par do
        v = await A;
        break;
    with
        v = await A;
        break;
    end;
end;
escape v;
]],
    _ana = {
        unreachs = 1,
        abrt = 3,
        acc = 1,     -- should be 0
    },
    run = {
        ['5~>A'] = 5,
    }
}
Test { [[
var int v1=0, v2=0;
loop do
    par do
        v1 = 1;
        break;
    with
        par/or do
            v2 = 2;
        with
        end;
        await FOREVER;
    end;
end;
escape v1 + v2;
]],
    _ana = {
        unreachs = 1,
        abrt = 4,
    },
    --run = 3,
    run = 1,
}
Test { [[
input int A;
var int v=0;
loop do
    par do
        v = await A;
        break;
    with
    end;
end;
escape v;
]],
    _ana = {
        unreachs = 1,
    },
    run = {
        ['5~>A'] = 5,
    }
}

Test { [[
input int A;
loop do
    await A;
    break;
    await A;
    break;
end;
]],
    --ast = "line 4 : after `;´ : expected `end´",
    parser = 'line 4 : after `;´ : expected `end´',
}

Test { [[
input int A;
var int v1=0,v2=0;
loop do
    par do
        v1 = await A;
        break;
    with
        v2 = await A;
        break;
    end;
end;
escape v1 + v2;
]],
    _ana = {
        unreachs = 1,
        abrt = 3,
    },
    run = {
        --['5~>A'] = 10,
        ['5~>A'] = 5,
    }
}

Test { [[
input int A;
var int v1=0, v2=0;
loop do
    par/or do
        await A;
    with
        v1 = await A;
        break;
    end;
    v2 = 2;
    break;
end;
escape 0;
]],
    _ana = {
        unreachs = 1,
        abrt = 3,
    },
    run = { ['1~>A']=0 },
}

Test { [[
input int A;
var int v1=0, v2=0, v3=0;
loop do
    par/or do
        v1 = await A;
    with
        v3 = await A;
        break;
    end;
    v2 = 1;
    break;
end;
escape v1+v2+v3;
]],
    _ana = {
        unreachs = 1,
        abrt = 3,
    },
    run = {
        --['2~>A'] = 5,
        ['2~>A'] = 3,
    }
}

-- TODO: parei com abrt aqui!!!
Test { [[
var int v1=0,v2=0,v3=0,v4=0,v5=0,v6=0;
loop do
    par/or do
        v1 = 1;
        break;
    with
        loop do
            par/or do
                v2 = 2;
            with
                v3 = 3;
                break;
            end;
            v4 = 4;
            break;
        end;
        v5 = 5;
    end;
    v6 = 6;
    break;
end;
escape v1+v2+v3+v4+v5+v6;
]],
    _ana = {
        unreachs = 2,
        abrt = 6,
    },
    --run = 21,
    run = 1,
}

Test { [[
var int v1=0,v2=0,v3=0,v4=0,v5=0,v6=0;
loop do
    par/or do
        v1 = 1;
    with
        loop do
            par/or do
                v2 = 2;
            with
                v3 = 3;
            end;
            v4 = 4;
            break;
        end;
        v5 = 5;
    end;
    v6 = 6;
    break;
end;
escape v1+v2+v3+v4+v5+v6;
]],
    _ana = {
        unreachs = 2,
        abrt = 6,
    },
    --run = 21,
    run = 7,
}

Test { [[
input void A;
var int v1=0,v2=0,v3=0,v4=0,v5=0,v6=0;
loop do
    par/or do
        await A;
        v1 = 1;
        break;
    with
        loop do
            par/or do
                await A;
                v2 = 2;
            with
                await A;
                v3 = 3;
                break;
            end;
            v4 = 4;
            break;
        end;
        v5 = 5;
    end;
    v6 = 6;
    break;
end;
escape v1+v2+v3+v4+v5+v6;
]],
    _ana = {
        unreachs = 2,
        abrt = 6,
    },
    --run = { ['~>A'] = 21 },
    run = { ['~>A'] = 1 },
}

Test { [[
input int A;
var int v1=0,v2=0,v3=0,v4=0,v5=0,v6=0;
loop do
    par/or do
        await A;    // 5
        v1 = 1;
    with
        loop do     // 8
            par/or do
                await A;    // 10
                v2 = 2;
            with
                await A;    // 13
                v3 = 3;
            end;
            v4 = 4;
            break;
        end;
        v5 = 5;
    end;
    v6 = 6;
    break;
end;
escape v1+v2+v3+v4+v5+v6;
]],
    _ana = {
        unreachs = 2,
        abrt = 6,
    },
    --run = { ['1~>A']=21 },
    run = { ['1~>A']=7 },
}

Test { [[
var int v1=0,v2=0,v3=0,v4=0,v5=0,v6=0;
loop do
    par do
        escape 1;           // acc 1
    with
        loop do
            par/or do
                v2 = 2;
                            // prio 1
            with
                v3 = 3;
                break;      // prio 1
            end;
            v4 = 4;
            break;
        end;
        escape 1;           // acc 1
    end;
end;
// unreachable
escape v1+v2+v3+v4+v5+v6;   // TODO: unreach
]],
    _ana = {
        unreachs = 3,
        acc = 1,
        abrt = 3,
    },
}

Test { [[
input int A,B;
var int v=0;
loop do
    await A;
    par/or do
        await A;
    with
        v = await B;
        break;
    end;
end;
escape v;
]],
    run = {
        ['1~>A ; 5~>B'] = 5,
        ['1~>A ; 1~>A ; 3~>B ; 1~>A ; 5~>B'] = 5,
    }
}

Test { [[
input int A,B,Z,X;
var int a = 0;
a = do/_ par do
    par/and do
        await A;
    with
        await B;
    end;
    escape a+1;
with
    await Z;
    escape a;
end
end;
a = a + 1;
await X;
escape a;
]],
    --ref = 'line 9 : invalid access to uninitialized variable "a" (declared at tests.lua:2)',
    run = { ['0~>A;0~>B;0~>Z;0~>X'] = 2 }
}

Test { [[
input int A,B,Z,X;
var int a = 0;
a = do/_ par do
    par/and do
        await A;
    with
        await B;
    end;
    escape 1;
with
    await Z;
    escape 0;
end
end;
a = a + 1;
await X;
escape a;
]],
    run = { ['0~>A;0~>B;0~>Z;0~>X'] = 2 },
    safety = 2,
    _ana = {
        acc = true,
    },
}

Test { [[
input int A,B,Z,X;
var int a = 0;
a = do/_ par do
    par do
        await A;
        escape 0;
    with
        await B;
        escape 0;
    end;
with
    await Z;
    escape 0;
end
end;
a = a + 1;
await X;
escape a;
]],
    run = { ['0~>A;0~>B;0~>Z;0~>X'] = 1 }
}

Test { [[
input int A,B,Z,X;
var int a = 0;
a = do/_ par do
    par do
        await A;
        escape 0;
    with
        await B;
        escape 0;
    end;
    // unreachable
with
    await Z;
    escape 0;
end
end;
a = a + 1;
await X;
escape a;
]],
    run = { ['0~>A;0~>B;0~>Z;0~>X'] = 1 }
}

Test { [[
input int B;
var int a = 0;
par/or do
    par/or do
        await B;
    with
        await B;
    end;
with
    await B;
end;
a = a + 1;
escape a;
]],
    _ana = {
        abrt = 2,
    },
    run = { ['0~>B'] = 1 }
}

Test { [[
input int B;
var int a = 1;
var int b=0;
loop do
    par/or do
        await B;
    with
        var int v = await B;
        b = v;
        break;
    end;
    b = a;
    break;
end;
a = a + 1;
escape a;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = { ['2~>B'] = 2 }
}

Test { [[
input int B;
var int a = 0;
loop do
    par/or do
        await B;
    with
        await B;
        a = a + 1;
        break;
    end;
    a = a + 1;
    break;
end;
a = a + 1;
escape a;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    --run = { ['2~>B'] = 3 }
    run = { ['2~>B'] = 2 }
}

Test { [[
input int B;
var int b = 0;
loop do
    par/and do
        await B;
        break;
    with
        await B;
        break;
    end;
    b = b + 1;
end;
escape b;
]],
    _ana = {
        --dfa = 'unreachable statement',
        unreachs = 3,
        abrt = 1,
    },
    run = { ['0~>B'] = 0 }
}

Test { [[
input int B;
var int b = 0;
loop do
    par do
        await B;
        break;
    with
        await B;
        break;
    end;
end;
escape b;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = { ['0~>B'] = 0 }
}

Test { [[
input int B;
var int a = 1;
par/or do
    await B;
with
    var int b = do/_ loop do
            par/or do
                await B;
                            // prio 1
            with
                var int v = await B;
                escape v;   // prio 1
            end;
            a = a + 1;
            escape a;
end;
        end;
    a = a + 2 + b;
end
escape a;
]],
    _ana = {
        abrt = 2,
        unreachs = 1,
    },
    --run = { ['10~>B'] = 6 },
    run = { ['10~>B'] = 1 },
}

Test { [[
input int B;
var int a = 1;
par/or do
    await B;
with
    var int b = do/_ loop do
            par/or do
                await B;
            with
                var int v = await B;
                escape v;
            end;
            a = a + 1;
        end;
end;
    a = a + 2 + b;
end
escape a;
]],
    _ana = {
        abrt = 2,
    },
    --run = { ['10~>B'] = 14 },
    run = { ['10~>B'] = 1 },
}

Test { [[
input int B;
var int a = 1;
loop do
    par/or do
        await B;
    with
        await B;
        break;
    end;
    a = a + 1;
    break;
end;
escape a;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = { ['10~>B'] = 2 },
}

Test { [[
input int B;
var int a = 1;
loop do
    par do
        await B;
        break;
    with
        await B;
        break;
    end;
end;
escape a;
]],
    _ana = {
        unreachs = 1,
        abrt = 1,
    },
    run = { ['0~>B'] = 1 }
}

Test { [[
input int B;
var int a = 1;
loop do
    par/or do
        await B;
        break;
    with
        await B;
        break;
    end;
    // unreachable
    a = a + 1;
    break;
end;
escape a;
]],
    _ana = {
        abrt = 2,
        unreachs = 3,
    },
    run = { ['0~>B'] = 1 }
}

Test { [[
input int B;
var int a = 1;
loop do
    par/and do
        await B;
        break;
    with
        await B;
        break;
    end;
    // unreachable
    a = a + 1;
    break;
end;
escape a;
]],
    _ana = {
        abrt = 1,
        unreachs = 3,
    },
    run = { ['0~>B'] = 1 }
}

-- pode inserir 2x na fila
Test { [[
input int B;
var int b=0;
var int a = 2;
par/or do
with
    a = a + 1;
end;
b = a;
a = a*2;
await B;
escape a;
]],
    _ana = {
        abrt = 1,
    },
    run = {
        --['0~>B'] = 6,
        ['0~>B'] = 4,
    }
}
Test { [[
input int B;
var int a = 2;
par/and do
with
    par/and do
    with
    end;
    a = a + 1;
end;
a = a * 2;
await B;
escape a;
]],
    run = {
        ['0~>B'] = 6,
    }
}
Test { [[
var int a;
a = 2;
par/and do
with
    par/and do
    with
    end;
end;
a = a * 2;
escape a;
]],
    run = 4,
}
Test { [[
var int a;
a = 2;
par/or do
with
    par/or do
    with
    end;
end;
a = a * 2;
escape a;
]],
    _ana = {
        abrt = 2,
    },
    run = 4,
}

Test { [[
var int a=0, b=0, c=0, d=0, e=0, f=0;
par/and do
    a = 1;
with
    par/and do
        par/and do
            b = 1;
        with
            c = 1;
        end;
    with
        par/and do
            d = 1;
        with
            e = 1;
        end;
    end;
with
    f = 1;
end;
escape a+b+c+d+e+f;
]],
    run = 6,
}

-- EX.12: gates do AND podem conflitar com ret do loop
Test { [[
input int A;
var int v=0;
loop do
    par/and do
        v = await A;
        break;
    with
        await A;
    end;
end;
escape v;
]],
    _ana = {
        --abrt = 1,
        unreachs = 2,
    },
    run = { ['5~>A'] = 5, }
}

Test { [[
input int A;
var int v=0;
loop do
    par/or do
        v = await A;
        break;
    with
        await A;
    end;
end;
escape v;
]],
    _ana = {
        abrt = 1,
    },
    run = { ['5~>A'] = 5, }
}

Test { [[
input int A;
var int v=0;
par/or do
    loop do
        v = await A;
        break;
    end;
    escape v;
with
    var int v = await A;
    escape v;
end;
]],
    _ana = {
        unreachs = 2,
        acc = 1,
        abrt = 2,
    },
}

Test { [[
input int A,B;
var int v=0;
par/or do
    loop do
        par/or do
            v = await A;
            break;
        with
            await B;
        end;
    end;
with
    v = await A;
end;
escape v;
]],
    _ana = {
        acc = 1, -- should be 0 (same evt)
        abrt = 1,
    },
    run = {
        ['0~>B ; 5~>A'] = 5,
    }
}

-- Testa prio em DFA.lua
Test { [[
input int A;
var int b=0,c=0,d=0;
par/or do
    par/and do
        loop do
            par/or do
                await A;
            with
                await A;
            end;
            b = 3;
            break;
        end;
    with
        await A;
        c = 3;
    end;
with
    await A;
    d = 3;
end;
escape b+c+d;
]],
    _ana = {
        unreachs = 1,
        abrt = 2,
    },
    --run = { ['0~>A'] = 9, }
    run = { ['0~>A'] = 6, }
}

Test { [[
input int A;
var int b=0,c=0,d=0;
par/or do
    par/and do
        loop do
            par/or do
                await A;
                break;
            with
                await A;
            end;
            b = 3;
        end;
    with
        await A;
        c = 3;
    end;
with
    await A;
    d = 3;
end;
escape b+c+d;
]],
    _ana = {
        abrt = 2,
    },
    --run = { ['0~>A'] = 9, }
    run = { ['0~>A'] = 3, }
}

Test { [[
input int A,Z,X;
var int b=0;
par/or do
    b = 0;
    loop do
        var int v=0;
        par/and do
            await A;
        with
            v = await A;
        end;
        b = 1 + v;
    end;
with
    await Z;
    await X;
    escape b;
end;
]],
    _ana = {
        unreachs = 1,
    },
    run = {
        ['2~>Z ; 1~>A ; 1~>X'] = 2,
    }
}

Test { [[
input void A,Z;
var int ret = 0;
par/or do
    loop do
        par/and do
        with
            await A;
        end;
        ret = ret + 1;
    end;
with
    await Z;
end;
escape ret;
]],
    _ana = {
        unreachs = 1,
    },
    run = { ['~>A;~>A;~>Z']=2 },
}
Test { [[
input int A;
input void X,Z;
var int b=0;
par/or do
    b = 0;
    loop do
        var int v=0;
        par/and do
        with
            v = await A;
        end;
        b = 1 + v;
    end;
with
    await Z;
    await X;
    escape b;
end;
]],
    _ana = {
        unreachs = 1,
    },
    run = { ['1~>A;~>Z;2~>A;~>X']=3 },
}

Test { [[
input int A;
var int c = 2;
var int d = par/and do
    with
        escape c;
    end;
c = d + 1;
await A;
escape c;
]],
    parser = 'line 3 : after `=´ : expected expression',
}

Test { [[
input int A;
var int c = 2;
var int d = do/_ par do
    with
        escape c;
end
    end;
c = d + 1;
await A;
escape c;
]],
    --abrt = 1,
    run = {
        ['0~>A'] = 3,
    }
}

    -- FRP
Test { [[
event int a,b;
par/or do
    emit a => 2;
with
    emit b => 5;
end;
escape 2;
]],
    _ana = {
        abrt = 1,
    },
    --run    = 7,
    run    = 2,
    --trig_wo = 2,
}

-- TODO: PAREI DE CONTAR unreachs AQUI
Test { [[
input int A;
var int counter=0;
event int c;
par/and do
    loop do
        await A;
        counter = counter + 1;
    end;
with
    loop do
        await c;
        // unreachable
        if counter == 200 then
            counter = 0;
        end;
    end;
end;
// unreachable
]],
    _ana = {
        isForever = true,
        unreachs = 3,
    },
}

Test { [[
input int A;
var int counter=0;
event int c;
par/and do
    loop do
        await A;
        counter = counter + 1;
    end;
with
    loop do
        await c;
        // unreachable
        if counter == 200 then
            counter = 0;
        end;
    end;
end;
// unreachable
]],
    safety = 2,
    _ana = {
        isForever = true,
        unreachs = 3,
        acc = 3,
    },
}

Test { [[
event int a;
emit a => 8;
escape 8;
]],
    run = 8,
    --trig_wo = 1,
}

Test { [[
event int a;
par/and do
    emit a => 9;
with
    loop do
        await a;
    end;
end;
]],
    _ana = {
        acc = 1,
        isForever = true,
        unreachs = 1,
        --trig_wo = 1,
    },
}

Test { [[
event int a;
par/and do
    emit a => 9;
with
    loop do
        await a;
    end;
end;
]],
    _ana = {
        acc = 1,
        isForever = true,
        unreachs = 1,
    },
}

Test { [[
input int A;
event int a,b;
var int v=0;
par/or do
    v = await A;
    par/or do
        emit a => 1;
    with
        emit b => 1;
    end;
    v = await A;
with
    loop do
        par/or do
            await a;
        with
            await b;
        end;
    end;
end;
escape v;
]],
    _ana = {
        abrt = 2,
    },
    run = {
        ['1~>A ; 1~>A'] = 1,
    }
}

Test { [[
input int X, E;
event int a, b;
var int c=0;
par/or do
    await X;
    par/or do
        emit a => 8;
    with
        emit b => 5;
    end;
    var int v = await X;
    escape v;
with
    c = 0;
    loop do
        var int aa=0,bb=0;
        par/or do
            aa=await a;
        with
            bb=await b;
        end;
        c = aa + bb;
    end;
with
    await E;
    escape c;
end;
]],
    _ana = {
        abrt = 2,
        unreachs = 1,
        acc = 0,
        --trig_wo = 1,
    },
    run = {
        ['1~>X ; 1~>E'] = 8,    -- TODO: stack change (8 or 5)
    }
}

Test { [[
input int X, E;
event int a, b;
var int c=0;
par/or do
    await X;
    par/or do
        emit a => 8;
    with
        emit b => 5;
    end;
    var int v = await X;
    escape v;
with
    c = 0;
    loop do
        var int aa=0,bb=0;
        par/or do
            aa=await a;
        with
            bb=await b;
        end;
        c = aa + bb;
    end;
with
    await E;
    escape c;
end;
]],
    safety = 2,
    _ana = {
        abrt = 2,
        unreachs = 1,
        acc = 3,
        --trig_wo = 1,
    },
    run = {
        ['1~>X ; 1~>E'] = 8,    -- TODO: stack change (8 or 5)
    },
}

Test { [[
var int v;
par/and do
    v = 1;
with
    v = 2;
end;
escape v;
]],
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:2)'
}

Test { [[
var int v;
par/and do
    v = 1;
with
end;
escape v;
]],
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:2)',
}

Test { [[
var int a;
loop do
    if false then
        a = 1;
    else
        do break; end
        a = 2;
    end
end
escape a;
]],
    wrn = true,
    ref = 'line 1 : uninitialized variable "a" crossing compound statement (tests.lua:2)',
}

Test { [[
var int v;
par/or do
with
    v = 1;
end;
escape v;
]],
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:2)',
}

Test { [[
input int A,B;
event int a,b;
var int v;
par/or do
    par/and do
        var int v = await A;
        emit a => v;
    with
        await B;
        emit b => 1;
    end;
    escape v;
with
    v = await a;
    escape v;       // 15
with
    var int bb = await b;
    escape bb;       // 18
end;
]],
    wrn = true,
    ref = 'line 3 : uninitialized variable "v" crossing compound statement (tests.lua:4)',
}

Test { [[
input int A,B;
event int a,b;
var int v=0;
par/or do
    par/and do
        var int v = await A;
        emit a => v;
    with
        await B;
        emit b => 1;
    end;
    escape v;
with
    v = await a;
    escape v;       // 15
with
    var int bb = await b;
    escape bb;       // 18
end;
]],
    _ana = {
        --nd_esc = 2,
        unreachs = 4,
    },
    run = {
        ['10~>A'] = 10,
        ['4~>B'] = 1,
    }
}

Test { [[
input int A,B;
event int a,b;
var int v=0;
par/or do
    par/and do
        var int v = await A;
        emit a => v;
    with
        await B;
        emit b => 1;
    end;
    escape v;
with
    v = await a;
    escape v;       // 15
with
    var int bb = await b;
    escape bb;       // 18
end;
]],
    safety = 2,
    _ana = {
        acc = 4,
        --nd_esc = 2,
        unreachs = 4,
    },
    run = {
        ['10~>A'] = 10,
        ['4~>B'] = 1,
    }
}

Test { [[
input int A, B;
event int a,b;
var int v=0;
par/or do
    par/and do
        var int a = await A;
        v = a;
        escape v;
    with
        await B;
        emit b => 1;
        escape v;
    end;
with
    var int aa = await a;
    escape aa;
with
    var int bb = await b;
    escape bb;
end;
]],
    _ana = {
        --nd_esc = 1,
    unreachs = 4,
    },
    run = {
        ['10~>A'] = 10,
        ['4~>B'] = 1,
    }
}

-- EX.08: join pode tirar o A da espera
Test { [[
input int A, B;
var int a=0;
par/and do
    loop do
        par/or do
            await A;
            var int x = a;
        with
            await B;
        end;
    end;
with
    await B;
    a = await A;
end;
]],
    _ana = {
        isForever = true,
        acc = 1,
        unreachs = 1,
    },
}

-- EX.07: o `and` executa 2 vezes
Test { [[
input int X;
event int a;
loop do
    var int v = await X;
    emit a => v;
end;
]],
    _ana = {
        isForever = true,
        --trig_wo = 1,
    },
}

Test { [[
input int A, X, E;
event int a, b, c;
var int cc=0;                   // 0: cc=0
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end;
with
    var int bb = 0;             // 0: cc=0/bb=0
    loop do
        var int v = await X;    // 1: v=1
        bb = v + bb;            // 1: bb=2
        emit b => bb;
    end;
with
    cc = 0;
    loop do
        var int aa=0,bb=0;
        par/or do
            aa = await a;
        with
            bb = await b;       // bb=2
        end;
        cc = aa+bb;             // cc=3
        emit c => cc;
    end;
with
    await E;
    escape cc;
end;
]],
    _ana = {
        unreachs = 1,
    },
    --trig_wo = 1,
    run = {
        ['1~>X ; 1~>X ; 3~>A ; 1~>X ; 8~>A ; 1~>E'] = 8,
    }
}

    -- Exemplo apresentacao RSSF
Test { [[
input int A, Z;
event int b, d, e;
par/and do
    loop do
        await A;
        emit b => 0;
        var int v = await Z;
        emit d => v;
    end;
with
    loop do
        var int dd = await d;
        emit e => dd;
    end;
end;
]],
    _ana = {
        isForever = true,
        unreachs = 1,
        --trig_wo = 2,
    },
}

    -- SLIDESHOW
Test { [[
input int A,Z,X;
var int i=0;
par/or do
    await A;
    escape i;
with
    i = 1;
    loop do
        var int o = do/_ par do
                await Z;
                await Z;
                var int c = await Z;
                escape c;
            with
                var int d = await X;
                escape d;
end
            end;
        if o == 0 then
            i = i + 1;
        else
            if o == 1 then
                i = i + 1;
            else
                i = i - 1;
            end;
        end;
    end;
end;
]],
    _ana = {
        unreachs = 1,
    },
    run = {
        [ [[
0~>Z ; 0~>Z ; 0~>Z ;  // 2
0~>Z ; 0~>Z ; 2~>X ;  // 1
0~>Z ; 1~>X ;         // 2
0~>Z ; 0~>Z ; 0~>Z ;  // 3
0~>Z ; 0~>Z ; 0~>Z ;  // 4
0~>Z ; 0~>Z ; 2~>X ;  // 3
1~>X ;                // 4
1~>X ;                // 5
1~>A ;                // 5
]] ] = 5
    }
}

Test { [[
input int A, B, Z, X;
var int v=0;
par/and do
    par/and do
        v = await A;
    with
        v = await B;
    end;
with
    par/or do
        await Z;
    with
        await X;
    end;
end;
escape v;
]],
    run = {
        ['0~>B ; 0~>B ; 1~>A ; 2~>Z'] = 1,
        ['0~>B ; 0~>B ; 1~>X ; 2~>A'] = 2,
    }
}
Test { [[
input int A;
var int a=0;
par/and do
    a = await A;
with
    par/or do
        await A;
    with
        await A;
    end;
    escape a;
end;
]],
    _ana = {
        unreachs = 1,
        acc = 1,
        abrt = 1,
    },
}
Test { [[
input int A;
event int a;
var int aa=0;
par/and do
    await A;
    emit a => 1;
with
    aa = await a;   // 8
with
    aa=await a;     // 10
end;
escape aa;
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['0~>A'] = 1,
    }
}

-- EX.01: dois triggers no mesmo ciclo
Test { [[
input int A;
event int a;
var int aa=0;
par/and do
    await A;
    emit a => 1;
with
    aa = await a;
    emit a => aa;
end;
escape aa;
]],
    run = {
        ['0~>A'] = 1,
    }
}
-- EX.03: trig/await + await
Test { [[
input int A;
event int a;
par/and do
    await A;
    emit a => 1;
with
    await a;
    await a;
    escape 1;
end;
]],
    _ana = {
        --isForever = true,
        unreachs = 2,
    },
}
-- EX.03: trig/await + await
Test { [[
input int A;
event int a, b;
par/and do
    await A;
    par/or do
        emit a => 1;
    with
        emit b => 1;
    end;
with
    par/or do
        await a;
    with
        await b;
    end;
    par/or do
        await a;
        // unreachable
    with
        await b;
        // unreachable
    end;
    // unreachable
end;
// unreachable
escape 0;
]],
    _ana = {
        --isForever = true,
        unreachs = 4,
        abrt = 3,
    },
}

-- EX.03: trig/await + await
Test { [[
input int A;
event int a,b;
par/and do
    await A;
    par/or do
        emit a => 1;
    with
        emit b => 1;
    end;
with
    par/and do
        await a;
    with
        await b;
    end;
    par/or do
        await a;
    // unreachable
    with
        await b;
        // unreachable
    end;
    // unreachable
end;
// unreachable
escape 0;
]],
    _ana = {
        --isForever = true,
        unreachs = 4,
        abrt = 2,
    },
}

Test { [[
input int A;
event int a;
par/and do
    await A;
    emit a => 1;
with
    await a;
    await a;
    // unreachable
with
    await A;
    await a;
end;
// unreachable
escape 0;
]],
    _ana = {
        acc = 1,
        --isForever = true,
        unreachs = 2,
    },
}

Test { [[
input int A;
event int a;
var int aa=3;
par/and do
    await A;
    emit a => 1;
    aa=1;
    emit a => 3;
    aa=3;
with
    aa = await a;
end;
escape aa;
]],
    run = { ['1~>A']=3 }
}

-- TODO: STACK
Test { [[
input int A;
event int a;
var int aa=0;
par/or do
    await A;
    emit a => 1;
    emit a => 3;
    aa = 3;
with
    await a;
    aa = await a;
    aa = aa + 1;
end;
escape aa;
]],
    run = { ['1~>A;1~>A']=3 }
    --run = { ['1~>A;1~>A']=4 }
}

Test { [[
input int A, B;
var int v=0;
par/and do
    par/or do
        v = await A;
    with
        v = await B;
    end;
with
    loop do
        v = await B;
        break;
    end;
end;
escape v;
]],
    _ana = {
        unreachs = 1,
    acc = 1,     -- should be 0
    },
    run = {
        ['5~>B ; 4~>B'] = 5,
        --['1~>A ; 0~>A ; 5~>B'] = 5,
    }
}

Test { [[
input int A;
event int a;
par/and do
    await A;
    emit a => 8;
with
    await a;
    await a;
    // unreachable
end;
// unreachable
escape 0;
]],
    _ana = {
        --isForever = true,
        unreachs = 2,
    },
}
Test { [[
input void OS_START;
input int A,B;
event int a;
par/and do
    par/or do
        await A;
    with
        await B;
    end;
    await B;
    emit a => 1;
with
    par/and do
    with
        await B;
    end;
    await a;
end;
escape 10;
]],
    _ana = {
        acc = 1,
        --isForever = true,
        --unreachs = 2,
    },
    run = { ['1~>B;1~>B']=10 },
}

Test { [[
input int A, B, Z;
event int a;
var int aa=0;
par/and do
    par/or do
        await A;
    with
        await B;
    end;
    await B;
    emit a => 10;
with
    par/or do
        await Z;
    with
        await B;
    end;
    aa = await a;
end;
escape aa;
]],
    _ana = {
        acc = 1,
    },
    run = { ['1~>B;1~>B']=10 },
}

Test { [[
input int A, B, Z;
event int a;
var int aa=0;
par/and do
    par/or do
        await A;
    with
        await B;
    end;
    await B;
    emit a => 10;
with
    par/or do
        await Z;
    with
        await B;
    end;
    aa = await a;
end;
escape aa;
]],
    _ana = {
        acc = 1,
    },
    run = { ['1~>B;1~>B']=10 },
}

Test { [[
input void A;
event int a,b;
par/and do
    await A;
    emit a => 1;
    await A;
    emit b => 1;
with
    await a;
    await b;
    escape 1;
end;
escape 0;
]],
    _ana = {
        --nd_esc = 1,
        unreachs = 2,
        --trig_wo = 1,
    },
    run = { ['~>A;~>A'] = 1 }
}

Test { [[
input int A, B, Z, X, E;
var int d=0;
par/or do
    await A;
with
    await B;
end;
await Z;
par/and do
    d = await X;
with
    await E;
end;
escape d;
]],
    run = {
        ['1~>A ; 0~>Z ; 9~>X ; 10~>E'] = 9,
        ['0~>B ; 0~>Z ; 9~>E ; 10~>X'] = 10,
    },
}
Test { [[
input void OS_START;
event int a;
var int aa=0;
par/and do
    await OS_START;
    emit a => 1;
with
    par/or do
    with
    end;
    aa = await a;
end;
escape aa;
]],
    _ana = {
        --acc = 1,
        abrt = 1,
    },
    run = 1,
}
Test { [[
event int a;
par/and do
    emit a => 1;
with
    par/or do
    with
        await a;
    end;
end;
escape 1;
]],
    _ana = {
        acc = 1,
        unreachs = 1,
    },
    run = 1,
}


--[[
    -- GTES vs QUEUES:
    - Q_TIMERS
    (1) timer A triggers
    (2) timer A is cancelled (par/or), but remains in Q
    (3) timer A is reached again in a loop
    (4) both gates are on now
    (5) PROBLEM! (buffer overflow and execution)
    - Q_ASYNCS: similar to Q_TIMERS
    - Q_INTRA
    (1) event event triggers, await/cont go to Q
    (2) they are cancelled (par/or), both remain in Q
    (3) they cannot be reached in the same _intl_
    (4) so the gates are tested to 0, and halt
    - Q_TRACKS: similar to Q_INTRA
]]

Test { [[
input void OS_START, A;
var int v = 0;
event void a,b;
par/or do
    loop do
        par/or do
            loop do
                await a;
                v = v + 1;
            end;
        with
            await b;
        end;
    end;
with
    await OS_START;
    emit b;
    emit b;
    await A;
    emit a;
    escape v;
end;
]],
    _ana = {
        unreachs = 1,
    },
    run = { ['~>A']=1 },
}

Test { [[
input void OS_START;
var int v = 0;
event int a, b;
par/or do
    loop do
        var int aa = await a;
        emit b => aa;
        v = v + 1;
    end
with
    await OS_START;
    emit a => 1;
    escape v;
end;
]],
    run = 1,
    _ana = {
        unreachs = 1,
    },
}

Test { [[
input void OS_START, Z;
event void a, b;
par/or do
    loop do
        par/or do
            await a;
            emit b;
        with
            await b;
        end;
    end;
with
    await OS_START;
    emit a;
    await FOREVER;
with
    await Z;
end;
escape 1;
]],
    run = { ['~>Z'] = 1 },
}

Test { [[
input void OS_START;
event int a, b;
par/or do
    loop do
        par/or do
            var int aa = await a;
            emit b => aa;
        with
            var int bb = await b;
            if bb then end;
        end;
    end;
with
    await OS_START;
    emit a => 1;
end;
escape 10;
]],
    run = 10,
}

Test { [[
input void OS_START,A;
var int v = 0;
var int x = 0;
event int a, b;
par/or do
    loop do
        par/or do
            var int aa = await a;
            emit b => aa;
            v = v + 1;
        with
            loop do
                var int bb = await b;
                if bb then
                    break;
                end;
            end;
        end;
        x = x + 1;
    end;
with
    await OS_START;
    emit a => 1;
    await A;
    emit a => 1;
    await A;
    emit a => 0;
    escape v+x;
end;
escape 10;
]],
    --nd_esc = 1,
    --run = { ['~>A;~>A'] = 1 },
    run = { ['~>A;~>A'] = 4 },
    _ana = {
        unreachs = 1,
    },
}

Test { [[
var int v1=0;
async do
    var int v = v1 + 1;
end;
escape 0;
]],
    locs = 'line 3 : internal identifier "v1" is not declared',
}

Test { [[
input int A,B,Z;
var int v1=0,v2=0;
par do
    loop do
        par/or do
            await B;
            async do
                var int v = v1 + 1;
            end;
        with
            await B;
            async do
                var int v = v2 + 1;
            end;
        with
            await A;
        end;
    end;
with
    await Z;
    v1 = 1;
    v2 = 1;
    escape v1 + v2;
end;
]],
    locs = 'line 8 : internal identifier "v1" is not declared',
}

Test { [[
var int v=2;
async (v) do
    var int a = v;
    if a then end;
end;
escape v;
]],
    run = 2,
}

Test { [[
var int v=2;
var int x=v;
var& int px = &x;
async (px, v) do
    px = v + 1;
end;
escape x + v;
]],
    run = 5,
}

Test { [[
var int a = 0;
async (a) do
    a = 1;
    do
    end
end
escape a;
]],
    run = 1,
}

Test { [[
var int a = 0;
async (a) do
    a = 1;
    do
    end
end
escape a;
]],
    run = 1,
}

Test { [[
input void Z;
var int v=2;
var int ret = 0;
par/or do
    async (ret,v) do        // nd
        ret = v + 1;
    end;
with
    v = 3;                  // nd
    await Z;
end
escape ret + v;
]],
    _ana = {
        acc = 1,
    },
    run = 7,
}

Test { [[
input int A,B,Z;
var int v1=0,v2=0;
par do
    loop do
        par/or do
            await B;
            async (v1) do
                var int v = v1 + 1;
                if v then end
            end;
        with
            await B;
            async (v2) do
                var int v = v2 + 1;
                if v then end
            end;
        with
            await A;
        end;
    end;
with
    await Z;
    v1 = 1;
    v2 = 1;
    escape v1 + v2;
end;
]],
    run = { ['1~>Z']=2 },
}

Test { [[
input int A,B,Z;
var int v1=0,v2=0;
par do
    loop do
        par/or do
            await B;
            async do
                var int v = v1 + 1;
            end;
        with
            await B;
            async do
                var int v = v2 + 1;
            end;
        with
            await A;
        end;
    end;
with
    await Z;
    v1 = 1;
    v2 = 1;
    escape v1 + v2;
end;
]],
    locs = 'line 8 : internal identifier "v1" is not declared',
}

Test { [[
input void A,Z;
var int v=0;
par do
    loop do
        par/or do
            await 10ms;
            v = v + 1;
        with
            await A;
        end;
    end;
with
    await Z;
    escape v;
end;
]],
    run = {
        ['~>A; ~>A; ~>25ms; ~>Z'] = 2,
    }
}

Test { [[
input int P2;
par do
    loop do
        par/or do
            var int p2 = await P2;
            if p2 == 1 then
                escape 0;
            end;
        with
            loop do
                await 200ms;
            end;
        end;
    end;
with
    async do
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 1;
    end;
    await FOREVER;      // TODO: ele acha que o async termina
end;
]],
    run = 0,
}

    -- MISC

Test { [[
var int v=0;
loop do
    par/and do
        par/or do
            loop do
                v = 1;
                await 400ms;
                v = 1;
                await 100ms;
            end;
        with
            await 1000ms;
        end;
    with
        par/or do
            loop do
                v = 1;
                await 600ms;
                v = 1;
                await 150ms;
            end;
        with
            await 1500ms;
        end;
    with
        par/or do
            loop do
                v = 1;
                await 800ms;
                v = 1;
                await 200ms;
            end;
        with
            await 2000ms;
        end;
    end;
end;
]],
    _ana = {
        --acc = 3,
        acc = 12,           -- TODO: not checked
        isForever = true,
    },
}

Test { [[
input void A, B;
var int aa=0, bb=0;
par/and do
    await A;
    var int a = 1;
    aa = a;
with
    var int b = 3;
    await B;
    bb = b;
end
escape aa+bb;
]],
    run = { ['~>A;~>B']=4 },
}

Test { [[
input int Z;
event int draw, occurring, sleeping;
var int x=0, vis=0;
par do
    await Z;
    escape vis;
with
    par/and do
        loop do
            await draw;
            x = x + 1;
        end;
    with
        loop do
            vis = await occurring;      // 15
        end;
    with
        loop do
            var int s=0;
            par/or do
                s = await sleeping;     // 21
            with
                s = await sleeping;     // 23
            end;
            if s== 0 then
                vis = 1;                // 26
            else
                vis = 0;                // 28
            end;
        end;
    with
        loop do
            await 100ms;
            emit draw => 1;
        end;
    with
        loop do
            await 100ms;
            emit sleeping => 1;
            await 100ms;
            emit occurring => 1;
        end;
    end;
end;
]],
    _ana = {
        unreachs = 1,
        acc = 3,
        abrt = 1,
    },
    run = { ['~>1000ms;1~>Z'] = 1 }
}

Test { [[
input int Z;
event int draw, occurring, sleeping;
var int x=0, vis=0;
par do
    await Z;
    escape vis;
with
    par/and do
        loop do
            await draw;
            x = x + 1;
        end;
    with
        loop do
            vis = await occurring;      // 15
        end;
    with
        loop do
            var int s=0;
            par/or do
                s = await sleeping;     // 21
            with
                s = await sleeping;     // 23
            end;
            if s== 0 then
                vis = 1;                // 26
            else
                vis = 0;                // 28
            end;
        end;
    with
        loop do
            await 100ms;
            emit draw => 1;
        end;
    with
        loop do
            await 100ms;
            emit sleeping => 1;
            await 100ms;
            emit occurring => 1;
        end;
    end;
end;
]],
    safety = 2,
    _ana = {
        unreachs = 1,
        acc = 6,
        abrt = 1,
    },
    run = { ['~>1000ms;1~>Z'] = 1 }
}

Test { [[
input void OS_START;
event int a, b;
var int v=0;
par/or do
    loop do
        await a;
        emit b => 1;
        v = 4;
    end;
with
    loop do
        await b;
        v = 3;
    end;
with
    await OS_START;
    emit a => 1;
    escape v;
end;
// unreachable
escape 0;
]],
    _ana = {
        unreachs = 1,
    },
    run = 4,
}

Test { [[
input void OS_START;
await OS_START;

native _pinMode, _digitalWrite;
native do
##define pinMode(a,b)
##define digitalWrite(a,b)
end
_pinMode(13, 1);
_digitalWrite(13, 1);
do escape 1; end

par do
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
with
    await OS_START;
end
]],
    wrn = true,
    run = 1,
}

Test { [[
input void OS_STOP;
var int ret = 0;

par/or do

input void OS_START;

await OS_START;

do finalize with
    nothing;
end

par do
    loop do
        await 10min;
    end
with
    await 1s;
    loop do
        par/or do
            loop do
                await 10min;
            end
        with
            await 1s;
            ret = ret + 1;
        end
    end
end

with
    await OS_STOP;
end

escape ret;
]],
    run = { ['~>OS_START; ~>10s; ~>OS_STOP']=9 },
}

Test { [[
var int ret = 0;
input void STOP;
par/or do
    loop do
        await 1s;
        ret = ret + 1;
    end

    loop do
        await 1s;
        par/or do with end
    end
with
    await STOP;
end
escape ret;
]],
    wrn = true,
    run = {['~>5s; ~>STOP']=5},
}

    -- SYNC TRIGGER

Test { [[
input void OS_START;
event int a;
var int v1=0, v2=0;
par/and do
    par/or do
        await OS_START;
        emit a => 10;
        v1=10;
    with
        await FOREVER;
    end;
with
    par/or do
        v2=await a;
        v2=v2+1;
    with
        await FOREVER;
    end;
end;
escape v1 + v2;
]],
    run = 21,
}

-- TODO: STACK
Test { [[
input void OS_START;
event int a;
var int aa=0;
par/or do
    loop do
        aa = await a;
        aa = aa + 1;
    end;
with
    await OS_START;
    emit a => 1;
    emit a => aa;
    emit a => aa;
    emit a => aa;
    emit a => aa;
    emit a => aa;
end;
escape aa;
]],
    --run = 7,
    run = 2,
}

Test { [[
input void OS_START,A;
event int a;
var int aa=0;
par/or do
    loop do
        aa=await a;
        aa = aa + 1;
    end;
with
    await OS_START;
    emit a => 1;
    await A;
    emit a => aa;
    await A;
    emit a => aa;
    await A;
    emit a => aa;
    await A;
    emit a => aa;
    await A;
    emit a => aa;
end;
escape aa;
]],
    run = { ['~>A;~>A;~>A;~>A;~>A'] = 7, },
}

Test { [[
input void OS_START, A;
event int a, b;
var int bb=0;
par/or do
    loop do
        bb=await b;
        bb = bb + 1;
    end;
with
    await a;
    emit b => 1;
    await A;
    emit b => bb;
    await A;
    emit b => bb;
    await A;
    emit b => bb;
    await A;
    emit b => bb;
    await A;
    emit b => bb;
    await A;
    emit b => bb;
with
    await OS_START;
    emit a => 1;
    bb = 0;
end;
escape bb;
]],
    _ana = {
        --nd_esc = 1,
        unreachs = 1,
    },
    run = 0,
}

Test { [[
input void OS_START;
event int a;
var int aa=0;
par/or do
    await OS_START;
    emit a => 0;
with
    aa = await a;
    aa= aa+1;
    emit a => aa;
    await FOREVER;
end;
escape aa;
]],
    run = 1,
}

Test { [[
input void OS_START;
event int a,b;
var int aa=0;
par/or do
    await OS_START;
    emit a => 0;
with
    aa=await a;
    aa=aa+1;
    emit b => aa;
    aa = aa + 1;
    await FOREVER;
with
    var int bb = await b;
    bb = bb + 1;
    await FOREVER;
end;
escape aa;
]],
    run = 2,
}

Test { [[
input int A, Z;
event int c;
var int cc = 0;
par do
    loop do
        await A;
        emit c => cc;
    end;
with
    loop do
        cc = await c;
        cc = cc + 1;
    end;
with
    await Z;
    escape cc;
end;
]],
    run = { ['1~>A;1~>A;1~>A;1~>Z'] = 3 },
}

Test { [[
input void OS_START;
input int A, Z;
event int c;
var int cc = 0;
par do
    loop do
        await A;
        emit c => cc;
    end;
with
    loop do
        cc = await c;
        cc = cc + 1;
    end;
with
    await Z;
    escape cc;
end;
]],
    run = { ['1~>A;1~>A;1~>A;1~>Z'] = 3 },
    safety = 2,
    _ana = {
        acc = 4,
    },
}

Test { [[
input void OS_START;
event int a;
par do
    loop do
        await OS_START;
        emit a => 0;
        emit a => 1;
        await 10s;
    end;
with
    var int v1=0,v2=0;
    par/and do
        v1 = await a;
    with
        v2 = await a;
    end;
    escape v1+v2;
end;
]],
    _ana = {
        --nd_esc = 1,
        unreachs = 3,
        --trig_wo = 1,  -- unreachs
    },
    run = 0,
}

Test { [[
input void OS_START, A;
event int a;
par do
    loop do
        await OS_START;
        emit a => 0;
        await A;
    emit a => 1;
        await 10s;
    end;
with
    var int v1,v2;
    v1 = await a;
    v2 = await a;
    escape v1 + v2;
end;
]],
    _ana = {
        --nd_esc = 1,
        unreachs = 2,
    },
    run = { ['~>A']=1 },
}

Test { [[
input int A;
event int c;
var int a=0;
par/or do
    loop do
        a = await c;
    end;
with
    await A;
    emit c => 1;
    a = 1;
end;
escape a;
]],
    run = { ['10~>A'] = 1 },
}

Test { [[
event int b, c;
var int a=0;
par/or do
    loop do
        var int cc = await c;        // 4
        emit b => cc+1;     // 5
        a = cc+1;
    end;
with
    loop do
        var int bb = await b;        // 10
        a = bb + 1;
    end;
with
    emit c => 1;           // 14
    a = 1;
end;
escape a;
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}

Test { [[
input int A, Z;
var int i = 0;
event int a, b;
par do
    par do
        loop do
            var int v = await A;
            emit a => v;
        end;
    with
        loop do
            var int aa = await a;
            emit b => aa;
            var int aa = await a;
            emit b => aa;
        end;
    with
        loop do
            var int bb = await b;
            emit a => bb;
            i = i + 1;
        end;
    end;
with
    await Z;
    escape i;
end;
]],
    wrn = true,
    run = { ['1~>A;1~>A;1~>A;1~>A;1~>A;1~>Z'] = 5 },
}

Test { [[
input void Z;
event int x;
event int y;
var int xx = 0;
var int yy = 0;
var int a = 0;
var int b = 0;
var int c = 0;
par do
    loop do
        await 100ms;
        par/or do
            xx = xx + 1;
            emit x => xx;
        with
            yy = yy + 1;
            emit y => yy;
        end;
    end;
with
    loop do
        par/or do
            var int xx = await x;
            a = a + xx;
        with
            var int yy = await y;
            b = b + yy;
        end;
        c = a + b;
    end;
with
    await Z;
    escape c;
end;
]],
    _ana = {
        abrt = 2,
    },
    run = { ['~>1100ms ; ~>Z'] = 66 }   -- TODO: stack change
}

Test { [[
input void Z;
event int x;
event int y;
var int xx = 0;
var int yy = 0;
var int a = 0;
var int b = 0;
var int c = 0;
par do
    loop do
        await 100ms;
        par/or do
            xx = xx + 1;
            emit x => xx;
        with
            yy = yy + 1;
            emit y => yy;
        end;
    end;
with
    loop do
        par/or do
            var int xx = await x;
            a = a + xx;
        with
            var int yy = await y;
            b = b + yy;
        end;
        c = a + b;
    end;
with
    await Z;
    escape c;
end;
]],
    safety = 2,
    _ana = {
        acc = 1,
        abrt = 2,
    },
    run = { ['~>1100ms ; ~>Z'] = 66 }   -- TODO: stack change
}

Test { [[
input void OS_START;
event int a, b, c;
var int x = 0;
var int y = 0;
par/or do
    await OS_START;
    emit a => 0;
with
    await b;
    emit c => 0;
with
    par/or do
        await a;
        emit b => 0;
    with
        par/or do
            await b;    // 17
            x = 3;
        with
            await c;    // 20
            y = 6;
        end;
    end;
end;
escape x + y;
]],
    _ana = {
        unreachs = 4,
        abrt = 1,
    },
    run = 6,    -- TODO: stack change (6 or 3)
}

Test { [[
input void Z;
event int x;
event int y;
var int xx = 0;
var int yy = 0;
var int a = 0;
var int b = 0;
var int c = 0;
par do
    loop do
        await 100ms;
        par/or do
            xx = xx + 1;
            emit x => xx;
        with
            yy=yy+1;
            emit y => yy;
        end;
        c = c + 1;
    end;
with
    loop do
        par/or do
            var int xx = await x;
            a = xx + a;
        with
            var int yy = await y;
            b = yy + b;
        end;
        c = a + b + c;
    end;
with
    await Z;
    escape c;
end;
]],
    _ana = {
        abrt = 2,
    },
    run = {
        ['~>99ms;  ~>Z'] = 0,
        ['~>199ms; ~>Z'] = 2,
        ['~>299ms; ~>Z'] = 6,
        ['~>300ms; ~>Z'] = 13,
        ['~>330ms; ~>Z'] = 13,
        ['~>430ms; ~>Z'] = 24,
        ['~>501ms; ~>Z'] = 40,
    }
}

Test { [[
input void OS_START;
event int a;
var int b=0;
par/and do
    await OS_START;
    emit a => 1;
    b = 1;
with
    var int aa = await a;
    b = aa + 1;
end;
escape b;
]],
    run = 1,
}
Test { [[
input void OS_START;
event int a;
var int b=0;
par/or do
    await OS_START;
    emit a => 1;
    b = 1;
with
    var int aa =await a;
    b = aa + 1;
end;
escape b;
]],
    _ana = {
        unreachs = 1,
    --nd_esc = 1,
    },
    run = 2,
}

Test { [[
input void OS_START;
event int a;
par do
    var int aa = await a;
    emit a => 1;
    escape aa;
with
    await OS_START;
    emit a => 2;
    escape 0;
end;
]],
    _ana = {
        --nd_esc = 1,
    unreachs = 1,
    --trig_wo = 1,
    },
    run = 2,
}

Test { [[
input void OS_START;
event int a, b;
var int aa=0;
par/or do
    loop do
        await a;
        emit b => 1;
    end;
with
    await OS_START;
    emit a => 1;
with
    await b;
    emit a => 2;
    aa = 2;
end;
escape aa;
]],
    _ana = {
        --nd_esc = 2,
        unreachs = 3,
        --trig_wo = 1,
    },
    run = 2,
}

-- TODO: STACK
Test { [[
input void OS_START;
event int a;
var int x = 0;
par do
    await OS_START;
    emit a => 1;
    emit a => 2;
    escape x;
with
    loop do
        await a;
        x = x + 1;
    end
end
]],
    --run = 2,
    run = 1,
}
Test { [[
input void OS_START;
event int a;
var int x = 0;
par do
    await OS_START;
    emit a => 1;
    emit a => 2;
    escape x;
with
    loop do
        await a;
        x = x + 1;
    end
end
]],
    --run = 2,
    run = 1,
    safety = 2,
    _ana = {
        acc = 1,
    },
}
Test { [[
input void OS_START, A;
event int a;
var int x = 0;
par do
    await OS_START;
    emit a => 1;
    await A;
    emit a => 2;
    escape x;
with
    await a;
    x = x + 1;
    await a;
    x = x + 1;
end
]],
    run = {['~>A']=2,},
}
Test { [[
input void OS_START, A;
event int a;
var int x = 0;
par do
    await OS_START;
    emit a => 1;
    await A;
    emit a => 2;
    escape x;
with
    await a;
    x = x + 1;
    await a;
    x = x + 1;
end
]],
    run = {['~>A']=2,},
    safety = 2,
    _ana = {
        acc = 2,
    },
}
Test { [[
event int a;
var int x = 0;
par do
    emit a  =>  1;
    escape x;
with
    loop do
        await a;
        x = x + 1;
    end
end
]],
    _ana = {
        acc = 1,
        --abrt = 1,
        unreachs = 0,
    },
}
Test { [[
input void OS_START;
event void a;
var int x = 0;
par/or do
    await OS_START;
    emit a =>  1;
    // unreachable
with
    await a;
    x = x + 1;
    await a;        // 11
    x = x + 1;
with
    await a;
    emit a;         // 15
    // unreachable
end
escape x;
]],
    _ana = {
        abrt = 1,
        acc = 1,
        unreachs = 2,
    },
    run = 1,
    env = 'line 6 : arity mismatch',
    --env = 'line 6 : non-matching types on `emit´ (void vs int)',
}

-- TODO: STACK
Test { [[
input void OS_START;
event int a;
var int x = 0;
par/or do
    await OS_START;
    emit a =>  1;
    // unreachable
with
    await a;
    x = x + 1;
    await a;        // 11
    x = x + 1;
with
    await a;
    emit a => 1;         // 15
    // unreachable
end
escape x;
]],
    _ana = {
        abrt = 1,
        acc = 1,
        unreachs = 2,
    },
    --run = 2,
    run = 1,
}

Test { [[
event int a, x, y, vis;
par/or do
    par/and do
        emit x => 1;
        emit y => 1;
    with
        loop do
            par/or do
                await x;
            with
                await y;
            end;
        end;
    end;
with
    emit a => 1;
    emit x => 0;
    emit y => 0;
    emit vis => 1;
    await FOREVER;
end;
]],
    _ana = {
        --acc = 1,
        acc = 6,     -- TODO: not checked
        --trig_wo = 2,
        unreachs = 2,
        isForever = true,
    },
}

-- TODO: STACK
Test { [[
input void OS_START;
event void x, y;
var int ret = 0;
par/or do
    par/and do
        await OS_START;
        emit x;         // 7
        emit y;         // 8
    with
        loop do
            par/or do
                await x;    // 12
                ret = 1;    // 13
            with
                await y;    // 15
                ret = 10;   // 16
            end;
        end;
    end;
with
    await OS_START;
    emit x;             // 22
    emit y;             // 23
end;
escape ret;
]],
    _ana = {
        abrt = 1,
        acc = 5,
        --acc = 4,
        --trig_wo = 2,
        unreachs = 1,
    },
    --run = 10,
    run = 1,
}

Test { [[
input void OS_START;
event int a, x, y;
var int ret = 0;
par do
    par/and do
        await OS_START;
        emit x => 1;           // 7
        emit y => 1;           // 8
    with
        par/or do
            await x;
            ret = ret + 3;  // 12
        with
            await y;
            ret = 0;        // 15
        end;
    end;
with
    await OS_START;
    ret = ret + 1;
    emit a => 1;
    ret = ret * 2;
    emit x => 0;               // 7
    ret = ret + 1;
    emit y => 0;               // 25
    ret = ret * 2;
    escape ret;
end;
]],
    _ana = {
        abrt = 1,
        acc = 4,
        --acc = 1,
        --trig_wo = 2,
        unreachs = 1,
    },
    run = 18,
}

Test { [[
event int a, x, y, vis;
par/or do
    par/and do
        emit x => 1;
        emit y => 1;
    with
        loop do
            par/or do
                await x;
            with
                await y;
            end;
        end;
    end;
with
    emit a => 1;
    emit x => 0;
    emit y => 0;
    emit vis => 1;
    await FOREVER;
end;
]],
    _ana = {
        acc = 6,
        --trig_wo = 2,
        unreachs = 2,
        isForever = true,
    },
}

-- TODO: STACK
Test { [[
input void OS_START;
input int Z;
event int x, w, y, z, a, vis;
var int xx=0, ww=0, yy=0, zz=0, aa=0, vvis=0;
par do
    loop do
        par/or do
            xx = await x;
            xx = xx + 1;
        with
            yy = await y;    // 10
            yy = yy + 1;
        with
            zz = await z;    // 13
            zz = zz + 1;
        with
            ww = await w;
            ww = ww + 1;
        end;
        aa = aa + 1;
    end;
with
    await OS_START;
    aa=1;
    emit a => aa;
    yy=1;
    emit y => yy;
    zz=1;
    emit z => zz;
    vvis=1;
    emit vis => vvis;
with
    await Z;
    escape aa+xx+yy+zz+ww;
end;
]],
    _ana = {
        abrt = 1,        -- false positive
        --trig_wo = 2,
        unreachs = 2,
    },
    --run = { ['1~>Z']=7 },
    run = { ['1~>Z']=5 },
}

    -- SCOPE / BLOCK

Test { [[do end;]],
    _ana = {
        reachs = 1,
        isForever = true,
    },
}
Test { [[do var int a=0; end;]],
    _ana = {
        reachs = 1,
        isForever = true,
    },
}
Test { [[
do
    var int a=0;
    if a then end;
    escape 1;
end;
]],
    run = 1
}

Test { [[
do
    var int a = 1;
    do
        var int a = 0;
        if a then end;
    end;
    escape a;
end;
]],
    wrn = true,
    run = 1,
}

Test { [[
input void A, B;
do
    var int a = 1;
    var int tot = 0;
    par/and do
        var int a = 2;
        await A;
        tot = tot + a;
    with
        var int a = 5;
        await B;
        tot = tot + a;
    end;
    escape tot + a;
end;
]],
    wrn = true,
    run = { ['~>A;~>B']=8 },
}

Test { [[
do
    var int a = 1;
    var int b = 0;
    do
        escape a + b;
    end;
end;
]],
    run = 1,
}

Test { [[
input void A, B;
do
    var int a = 0;
    par/or do
        await A;
        a = 1;
        await A;
    with
        a = 2;
        await B;
    end;
    escape a;
end;
]],
    run = { ['~>A;~>B']=1 },
}

Test { [[
input void A, B;
var int a=0;
par/or do
    var int a;
    await A;
    a = 1;
    await A;
    if a then end;
with
    a = 2;
    await B;
end;
escape a;
]],
    wrn = true,
    run = { ['~>A;~>B']=2 },
}

Test { [[
do
    var int b=0;
    par/or do
        do b=1; end;
    with
        do b=2; end;
    end;
end;
escape 0;
]],
    _ana = {
        acc = 1,
        abrt = 1,
    },
}

Test { [[
input void A, B;
var int i=0;
do
    par/or do
        i = 0;
        await A;
        i = i + 1;
    with
    end;
end;
await B;
escape i;
]],
    _ana = {
        unreachs = 1,
    },
    run = {
        ['~>B'] = 0,
        ['~>A ; ~>B'] = 0,
        ['~>A ; ~>A ; ~>B'] = 0,
    }
}

Test { [[
event a;
escape 0;
]],
    parser = 'line 1 : after `event´ : expected type',
}

Test { [[
var int ret=0;
event int a;
par/or do
    do
        var int a = 0;
        par/or do
            par/or do
                emit a => 40;
            with
            end;
        with
            await a;
            ret = a;
        end;
    end;
    do
        var int a = 0;
        await a;
        ret = a;
    end;
with
input void A;
    a = await A;
end;
escape a;
]],
    wrn = true,
    env = 'line 8 : identifier "a" is not an event (tests.lua : line 5)',
}

Test { [[
var int ret = 0;
event void a,b;
par/or do
    ret = ret + 1;
with
    par/or do
        await a;    // 7
        emit b;
    with
        await b;
    end
with
    emit a;         // 13
end
par/and do
    emit a;
with
    emit b;
with
    ret = ret + 1;
end
escape ret;
]],
    _ana = {
        abrt = 1,
        acc = 1,
    },
    run = 2,
}

Test { [[
var int ret = 0;
event void a;
par/or do
    ret = ret + 1;
with
    emit a;
with
    emit a;
end
par/and do
    emit a;
with
    emit a;
with
    ret = ret + 1;
end
escape ret;
]],
    _ana = {
        acc = 2,
        abrt = 3,
    },
    run = 2,
}

Test { [[
input int A;
var int ret=0;
var int aa=0;
par/or do
    do
        event int aa;
        par/or do
            par/or do
                emit aa => 1;  // 9
            with
            end;
        with
            ret = await aa;        // 13
        end;
    end;
    do
        event int aa;
        ret = await aa;
    end;
with
    aa = await A;
end;
escape aa;
]],
    _ana = {
    abrt = 1,
        --nd_esc = 2,
        unreachs = 3,
        acc = 1,
    },
    run = { ['10~>A']=10 },
}

Test { [[
input void OS_START;
var int ret=0;
par/or do
    event int a;
    par/or do
        await OS_START;
        emit a => 5;
        // unreachable
    with
        ret =await a;
    end;
with
    event int a;
    await a;
    // unreachable
    ret = 0;
end;
escape ret;
]],
    _ana = {
        --nd_esc = 1,
    unreachs = 2,
    },
    run = 5,
}

-->>> ALIASES / REFERENCES / REFS / &

Test { [[
var int a = 1;
var& int b = &a;
a = 2;
b = b+a;
escape a+b;
]],
    run = 8,
}
Test { [[
var int a = 1;
var& int b = &a;
b = &a;
a = 2;
escape b;
]],
    ref = 'line 3 : invalid attribution : variable "b" is already bound',
}
Test { [[
var int a = 1;
var& int b = a;
a = 2;
escape b;
]],
    ref = 'line 2 : invalid attribution : missing alias operator `&´ on the right',
}

Test { [[
var int a = 1;
var int b = 10;
var& int c;
if a==1 then
    c = &a;
else
    c = &b;
end
c = 100;
escape a+b;
]],
    run = 110,
}
Test { [[
var int a = 1;
var int b = 10;
var& int c;
if a==0 then
    c = &a;
else
    c = &b;
end
c = 100;
escape a+b;
]],
    run = 101,
}
Test { [[
var int a = 1;
var int b = 10;
var& int c;
if a==1 then
    c = &a;
    c = 100;
else
    c = &b;
end
escape a+b;
]],
    ref = 'line 6 : invalid extra access to variable "c" inside the initializing `if-then-else´ (tests.lua:4)',
}
Test { [[
var int a = 1;
var int b = 10;
var& int c;
if a==1 then
    c = &a;
else
    c = &b;
    c = 100;
end
escape a+b;
]],
    ref = 'line 8 : invalid extra access to variable "c" inside the initializing `if-then-else´ (tests.lua:4)',
}

Test { [[
native _V;
native do
    int V = 10;
end
var& int v = &_V;
escape v;
]],
    --gcc = 'error: assignment makes pointer from integer without a cast',
    run = 10;
}

Test { [[
native _V, _f;
native do
    int V = 0;
    void f (int* v) {
        *v = 10;
    }
end
_f(&&_V);
escape _V;
]],
    run = 10,
}

Test { [[
var int a = 1;
var& int b = &&a;
a = 2;
escape b;
]],
    env = 'line 2 : types mismatch (`int&´ <= `int&&´)',
    --run = 2,
}
Test { [[
var int x = 10;
var& int y = &&x;
escape y;
]],
    env = 'line 2 : types mismatch (`int&´ <= `int&&´)',
}

Test { [[
native _V;
native do
    int V = 10;
end
var& int v;
v = &_V;
escape v;
]],
    --gcc = 'error: assignment makes pointer from integer without a cast',
    --env = 'line 5 : invalid attribution (int& vs _&&)',
    run = 10;
}

Test { [[
native _Tx;
native do
    int f (int v) {
        escape v + 1;
    }
    typedef struct {
        int (*f) (int);
    } tp;
    tp Tx = { f };
end
var int v = 1;
var& int ref = &v;
escape _Tx.f(v);
]],
    run = 2,
}

Test { [[
var int a = 1;
var& int b;
escape b;
]],
    --ref = 'line 3 : reference must be bounded before use',
    ref = 'line 3 : invalid access to uninitialized variable "b"',
    --run = 2,
}
Test { [[
var int a = 1;
var& int b;
b = &a;
a = 2;
escape b;
]],
    run = 2,
}
Test { [[
native _V;
native do
    int V = 10;
end
var& int v;
v = &_V;
escape v;
]],
    --gcc = 'error: assignment makes pointer from integer without a cast',
    run = 10;
}

Test { [[
var& int a;
var int&& b = null;
a = b;
await 1s;
var int&& c = a;
escape 1;
]],
    env = 'line 3 : types mismatch (`int&´ <= `int&&´)',
    --run = { ['~>1s']=1 },
}
Test { [[
native _V;
escape 0;
]],
    tops = 'line 1 : native "_V declared but not used',
}
Test { [[
native do
    int V = 10;
end
var int vv = 10;
var& int v;
v = &&vv;
await 1s;
do
    var int vvv = 1;
end
escape *v;
]],
    env = 'line 6 : types mismatch (`int&´ <= `int&&´)'
}
Test { [[
native do
    int V = 10;
end
var int vv = 10;
var& int v;
v = vv;
await 1s;
do
    var int vvv = 1;
end
escape *v;
]],
    env = 'line 11 : invalid operand to unary "*"',
    --run = { ['~>1s']=10 };
}
Test { [[
native do
    int V = 10;
end
var int vv = 10;
var& int v;
v = &vv;
await 1s;
do
    var int vvv = 1;
    native/nohold ___ceu_nothing;
    ___ceu_nothing(&&vvv);
end
escape v;
]],
    run = { ['~>1s']=10 };
}

Test { [[
vector[] int v;
escape 1;
]],
    run = 1,
}

Test { [[
var int a=1, b=2;
var& int v;
if true then
else
    v = &b;
end
v = 5;
escape a + b + v;
]],
    --ref = 'line 5 : reference must be bounded in the other if-else branch',
    ref = 'line 5 : missing initialization for variable "v" in the other branch of the `if-then-else´ (tests.lua:3)',
}
Test { [[
var int a=1, b=2;
var& int v;
if true then
    v = &a;
else
end
v = 5;
escape a + b + v;
]],
    ref = 'line 4 : missing initialization for variable "v" in the other branch of the `if-then-else´ (tests.lua:3)',
}
Test { [[
var int a=1, b=2;
var& int v;
if true then
    v = &a;
else
    v = &b;
end
var& int x;
if false then
    x = &a;
else
    x = &b;
end
v = 5;
x = 1;
escape a + b + x + v;
]],
    run = 12,
}

Test { [[
native _V1, _V2;
native do
    int V1 = 10;
    int V2 = 5;
end
var& int v;
if true then
    v = &_V1;
else
    v = &_V2;
end
v = 1;
escape _V1+_V2;
]],
    --gcc = 'error: assignment makes pointer from integer without a cast',
    run = 6,
}

Test { [[
var int a=1, b=2, c=3;
var& int v;
if true then
    v = &a;
else
    v = &b;
end
var& int x;
if false then
    x = &a;
else/if true then
    x = &b;
else
    x = &c;
end
v = 5;
x = 1;
escape a + b + x + v;
]],
    run = 12,
}

Test { [[
var int a=1, b=2, c=3;
var& int v;
if true then
    v = &a;
else
    v = &b;
end
var& int x;
if false then
    x = &a;
else
    if true then
        x = &b;
    else
        x = &c;
    end
end
v = 5;
x = 1;
escape a + b + x + v;
]],
    run = 12,
}

Test { [[
var int v = 10;
loop do
    var& int i = &v;
    i = i + 1;
    break;
end
escape v;
]],
    wrn = true,
    run = 11,
}

Test { [[
var int v = 10;
var& int i;
loop do
    i = &v;
    i = i + 1;
    break;
end
escape v;
]],
    wrn = true,
    --ref = 'reference declaration and first binding cannot be separated by loops',
    ref = 'line 2 : uninitialized variable "i" crossing compound statement (tests.lua:3)',
}

Test { [[
var int v = 10;
loop do
    var& int? i = &v;
    i = i + 1;
    break;
end
escape v;
]],
    wrn = true,
    env = 'line 4 : invalid operands to binary "+"',
}

Test { [[
var int v = 10;
escape v!;
]],
    env = 'line 2 : not an option type',
}

Test { [[
var int v = 10;
var& int? i;
i = &v;
i = &v;
escape i!;
]],
    ref = 'line 4 : invalid attribution : variable "i" is already bound',
}

Test { [[
var int v = 10;
var& int? i;
loop do
    i = &v;
    i! = i! + 1;
    break;
end
escape v;
]],
    ref = 'line 4 : invalid attribution : variable "i" is already bound',
}
Test { [[
var int v = 10;
var& int? i;
i! = &v;
escape i!;
]],
    ref = 'line 3 : invalid attribution : cannot bind with operator `!´',
}

Test { [[
var int v = 10;
var& int? i;
loop do
    i! = &v;
    i! = i! + 1;
    if true then
        break;
    else
        await 1s;
    end
end
escape v;
]],
    ref = 'line 4 : invalid attribution : variable "i" is already bound',
    --run = 11,
    --ref = 'reference declaration and first binding cannot be separated by loops',
    --ref = 'line 2 : uninitialized variable "i" crossing compound statement (tests.lua:3)',
}

Test { [[
do
    sfc = &_TTF_RenderText_Blended();
finalize () with
    _SDL_FreeSurface(&&(sfc!));
end
]],
    parser = 'line 3 : after `(´ : expected expression',
}

Test { [[
do
    nothing;
finalize with
    nothing;
end
escape 0;
]],
    fin = 'TODO',
}

Test { [[
native _SDL_Surface, _TTF_RenderText_Blended, _SDL_FreeSurface;
var& _SDL_Surface? sfc;
every 1s do
    do
        sfc = &_TTF_RenderText_Blended();
    finalize (sfc) with
        _SDL_FreeSurface(&&(sfc!));
    end
end
escape 1;
]],
    ref = 'line 4 : invalid attribution : variable "sfc" is already bound',
    --ref = 'line 4 : reference declaration and first binding cannot be separated by loops',
    --ref = 'line 1 : uninitialized variable "sfc" crossing compound statement (tests.lua:2)',
}

Test { [[
native _fff;
native do
    int V = 10;
    int* fff (int v) {
        V += v;
        escape &V;
    }
end
var int   v = 1;
var int&& p = &&v;
var& int? r;
do r = &_fff(*p);
finalize (r) with
    nothing;
end
escape r;
]],
    env = 'line 16 : types mismatch (`int´ <= `int&?´)',
}

Test { [[
native _fff;
native do
    int V = 10;
    int* fff (int v) {
        V += v;
        escape &V;
    }
end
var int   v = 1;
var int&& p = &&v;
var& int? r;
do r = &_fff(*p);
finalize (r) with
    nothing;
end
escape r!;
]],
    run = 11,
}

Test { [[
var& int v;
do
    var int x;
    v = &x;
end
escape 1;
]],
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:2)',
    --ref = 'line 4 : invalid access to uninitialized variable "x" (declared at tests.lua:3)',
}

Test { [[
var& int v;
do
    var int x=1;
    v = &x;
end
escape 1;
]],
    --ref = 'line 4 : invalid attribution : variable "x" has narrower scope than its destination',
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:2)',
    --run = 1,
}

Test { [[
var& int v;
    var int x=1;
    v = &x;
escape v;
]],
    run = 1,
}

Test { [[
data Vx with
    var int v;
end

var& Vx v1 = Vx(1);
var& Vx v2, v3;
    v2 = Vx(2);
    v3 = Vx(3);
escape v1.v+v2.v+v3.v;
]],
    ref = 'line 5 : invalid attribution : missing alias operator `&´',
    --run = 6,
}

Test { [[
data Vx with
    var int v;
end

var Vx v1_ = Vx(1);
var& Vx v1 = &v1_;
var& Vx v2, v3;
do
    var Vx v2_ = Vx(2);
    v2 = &v2_;
end
do
    var Vx v3_ = Vx(3);
    v3 = &v3_;
end
escape v1.v+v2.v+v3.v;
]],
    ref = 'line 7 : uninitialized variable "v2" crossing compound statement (tests.lua:8)',
    --ref = 'line 10 : attribution to reference with greater scope',
    --ref = 'line 10 : invalid attribution : variable "v2_" has narrower scope than its destination',
    --run = 6,
}

Test { [[
native _SDL_Renderer, _f;
native/nohold _g;

var& _SDL_Renderer? ren;
    do ren = &_f();
    finalize with
    end

await 1s;
_g(&&(ren!));

escape 1;
]],
    gcc = 'error: unknown type name ‘SDL_Renderer’',
}

-->>> FINALLY / FINALIZE

Test { [[
native _Cnt;
    native/pure _Radio_getPayload;
    native/plain _message_t;
    var _message_t msg={};
    loop do
        await 1s;
        var _Cnt&& snd = _Radio_getPayload(&&msg, sizeof(_Cnt));
    end
]],
    --fin = 'line 5 : pointer access across `await´',
    _ana = {
        isForever = true,
    },
}
Test { [[
native _Cnt;
    native/plain _message_t;
    native/pure _Radio_getPayload;
    var _message_t msg={};
    loop do
        await 1s;
        var _Cnt&& snd = _Radio_getPayload(&&msg, sizeof(_Cnt));
    end
]],
    _ana = {
        isForever = true,
    },
}
Test { [[
do
do finalize with nothing; end
end
escape 1;
]],
    run = 1,
}

Test { [[
do finalize with
    do escape 1; end;
end
escape 0;
]],
    props = 'line 2 : not permitted inside `finalize´',
}

Test { [[
native _malloc;
var int&& ptr = _malloc();
]],
    fin = 'line 1 : must assign to a option reference (declared with `&?´)',
}

Test { [[
native _f;
do
    var int&& a;
    do a = _f();
    finalize with
        do await FOREVER; end;
    end
end
]],
    fin = 'line 5 : must assign to a option reference (declared with `&?´)',
}

Test { [[
native _f;
do
    var& int? a;
    do a = &_f();
    finalize with
        do await FOREVER; end;
    end
end
]],
    props = "line 7 : not permitted inside `finalize´",
}

Test { [[
native _f;
do
    var& int? a;
    do a = &_f();
    finalize with
        async do
        end;
    end
end
]],
    props = "line 7 : not permitted inside `finalize´",
}

Test { [[
native _f;
do
    var& int? a;
    do a = &_f();
    finalize with
        do escape 0; end;
    end
end
]],
    props = "line 7 : not permitted inside `finalize´",
}

Test { [[
loop do
    do
    var int&& a;
        var int&& b = null;
            a = b;
    end
end
]],
    tight = 'line 1 : tight loop', -- TODO: par/and
    --props = "line 8 : not permitted inside `finalize´",
    --fin = 'line 6 : attribution does not require `finalize´',
    --fin = 'line 6 : attribution to pointer with greater scope',
}

Test { [[
var int v = 10;
var int&& ptr = &&v;
await 1s;
escape *ptr;
]],
    fin = 'line 4 : unsafe access to pointer "ptr" across `await´',
}

Test { [[
var int&& a;
var int&& b = null;
a = b;
await 1s;
var int&& c = a;
escape 1;
]],
    fin = 'line 5 : unsafe access to pointer "a" across `await´',
}

Test { [[
var int&& a;
var int&& b = null;
a = b;
await 1s;
var int&& c = a;
escape 1;
]],
    fin = 'line 5 : unsafe access to pointer "a" across `await´',
}

Test { [[
var int v = 1;
var int&& x = &&v;
loop i in [0 |> 10[ do
    *x = *x + 1;
    await 1s;
end
escape v;
]],
    fin = 'line 4 : unsafe access to pointer "x" across `loop´ (tests.lua : 3)',
}

Test { [[
event void e;
var int v = 1;
var int&& x = &&v;
loop i in [0 |> 10[ do
    *x = *x + 1;
    emit e;
end
escape v;
]],
    fin = 'line 5 : unsafe access to pointer "x" across `loop´ (tests.lua : 4)',
}

Test { [[
event void e;
var int v = 1;
var int&& x = &&v;
loop i in *x do
    await 1s;
end
escape v;
]],
    run = { ['~>1s']=1 },
}

Test { [[
native _f;
input void E;
var& int? n;
do this.n = &_f();
finalize with
end
await E;
escape this.n!;
]],
    gcc = 'error: implicit declaration of function ‘f’',
}

Test { [[
loop do
    do
    var int&& a;
        var int&& b = null;
        do a = b;
        finalize with
            do break; end;
        end
    end
end
]],
    --loop = 'line 1 : tight loop', -- TODO: par/and
    --props = "line 8 : not permitted inside `finalize´",
    fin = 'line 6 : attribution does not require `finalize´',
    --fin = 'line 6 : attribution to pointer with greater scope',
}

Test { [[
loop do
    do
    var int&& a;
        var int&& b = null;
        do a := b;
        finalize with
            do break; end;
        end
    end
end
]],
    --loop = 'line 1 : tight loop', -- TODO: par/and
    --props = "line 8 : not permitted inside `finalize´",
    fin = 'line 6 : attribution does not require `finalize´',
}

Test { [[
var int ret = 0;
do
    var int b;
    do finalize with
        do
            a = 1;
            loop do
                break;
            end
            ret = a;
        end;
    end
end
escape ret;
]],
    locs = 'line 6 : internal identifier "a" is not declared',
}

Test { [[
var int ret =
do/_
    escape 1;
end;
escape ret;
]],
    run = 1,
}

Test { [[
native _f;
native do
    int* f (void) {
        escape NULL;
    }
end
var int r = 0;
do
    var& int? a;
    do a = &_f();
    finalize with
        var int b = do/_ escape 2; end;
    end
    r = 1;
end
escape r;
]],
    --props = "line 8 : not permitted inside `finalize´",
    gcc = '9:27: error: variable ‘__ceu_a_3’ set but not used [-Werror=unused-but-set-variable]',
}

Test { [[
native _f;
native do
    int* f (void) {
        escape NULL;
    }
end
var int r = 0;
do
    var& int? a;
    do a = &_f();
    finalize with
        if a? then end
        var int b = do/_ escape 2; end;
    end
    r = 1;
end
escape r;
]],
    --props = "line 8 : not permitted inside `finalize´",
    run = 1,
}

Test { [[
native _v;
var int a;
do _v(&&a);
finalize with
    nothing;
end
escape(a);
]],
    ref = 'line 2 : invalid access to uninitialized variable "a"',
}

Test { [[
native _f;
native do
    void f (int* a) {
        *a = 10;
    }
    typedef void (*t)(int*);
end
native _t;
var _t v = _f;
await 1s;
var int a=0;
do v(&&a);
finalize with nothing; end;
escape(a);
]],
    --env = 'line 8 : native variable/function "_f" is not declared',
    --fin = 'line 8 : attribution to pointer with greater scope',
    fin = 'line 11 : unsafe access to pointer "v" across `await´',
    --run = { ['~>1s']=10 },
}

Test { [[
native _f;
native do
    void f (int* a) {
        *a = 10;
    }
    typedef void (*t)(int*);
end
native _t;
var _t v = _f;
await 1s;
var int a=0;
do v(&&a); finalize with nothing; end;
escape(a);
]],
    --env = 'line 8 : native variable/function "_f" is not declared',
    --fin = 'line 8 : attribution to pointer with greater scope',
    fin = 'line 11 : unsafe access to pointer "v" across `await´',
}
Test { [[
native _f;
native do
    void f (int* a) {
        *a = 10;
    }
    typedef void (*t)(int*);
end
native _t;
var _t v = _f;
    native/nohold ___ceu_nothing;
    ___ceu_nothing(v);
await 1s;
do
    var int a=0;
    do _f(&&a); finalize with nothing; end;
    escape(a);
end
]],
    --env = 'line 8 : native variable/function "_f" is not declared',
    --fin = 'line 8 : attribution to pointer with greater scope',
    --fin = 'line 11 : pointer access across `await´',
    run = { ['~>1s']=10 },
}

Test { [[
native _f;
native do
    void f (int* a) {
        *a = 10;
    }
    typedef void (*t)(int*);
end
native _t;
var _t v = _f;
    native/nohold ___ceu_nothing;
    ___ceu_nothing(v);
await 1s;
var int a=0;
do _f(&&a); finalize with nothing; end;
escape(a);
]],
    --env = 'line 8 : native variable/function "_f" is not declared',
    --fin = 'line 8 : attribution to pointer with greater scope',
    --fin = 'line 11 : pointer access across `await´',
    run = { ['~>1s']=10 },
}

Test { [[
native _f;
pre native do
    void f (int* a) {
        *a = 10;
    }
    typedef void (*t)(int*);
end
native _t;
var _t v = _f;
var int a=0;
do v(&&a); finalize with nothing; end;
escape(a);
]],
    --env = 'line 8 : native variable/function "_f" is not declared',
    run = 10,
}

Test { [[
native _getV;
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v;
do
    v = &_getV();
finalize
with
    nothing;
end

escape v!;
]],
    run = 10,
}
Test { [[
native _V, _getV;
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v1;
do v1 = &_getV();
finalize with
    nothing;
end
v1 = 20;

var& int? v2;
do v2 = &_getV();
finalize with
    nothing;
end

escape v1!+v2!+_V;
]],
    env = 'line 14 : invalid attribution : missing `!´ (in the left) or `&´ (in the right)',
    --run = 60,
}
Test { [[
native _V, _getV;
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v1;
do
    v1 = &_getV();
finalize
with
    nothing;
end
v1! = 20;

var& int? v2;
do
    v2 = &_getV();
finalize
with
    nothing;
end

escape v1!+v2!+_V;
]],
    run = 60,
}

--<<< FINALLY / FINALIZE

Test { [[
native _t;
native do
    typedef struct t {
        int* ptr;
    } t;
    int* f (int* ptr) {
        escape ptr;
    }
end
var int v = 10;
var _t t;
escape *(t.ptr);
]],
    ref = 'line 12 : invalid access to uninitialized variable "t" (declared at tests.lua:11)',
    --run = 10,
}
Test { [[
native _t;
native/pure _f;
native do
    typedef struct t {
        int* ptr;
    } t;
    int* f (int* ptr) {
        escape ptr;
    }
end
var int v = 10;
var _t t;
t.ptr = &_f(&&v);
escape *(t.ptr);
]],
    env = 'line 12 : invalid attribution : l-value cannot hold an alias',
    --ref = 'line 12 : invalid access to uninitialized variable "t" (declared at tests.lua:11)',
    --run = 10,
}

Test { [[
code/instantaneous get (void)=>int&& do
    var int x;
    escape &&x;
end
escape 10;
]],
    parser = 'line 1 : after `code/instantaneous´ : expected `/recursive´ or abstraction identifier',
    --ref = 'line 3 : invalid access to uninitialized variable "x" (declared at tests.lua:2)',
}

Test { [[
code/instantaneous Get (void)=>int&& do
    var int x;
    escape &&x;
end
escape 10;
]],
    wrn = true,
    env = 'line 3 : invalid escape value : local reference',
    --ref = 'line 3 : invalid access to uninitialized variable "x" (declared at tests.lua:2)',
}

Test { [[
code/instantaneous Get (void)=>int&& do
    var int x=0;
    escape &&x;
end
escape 10;
]],
    wrn = true,
    env = 'line 3 : invalid escape value : local reference',
    --fin = 'line 3 : attribution to pointer with greater scope',
}

Test { [[
code/instantaneous Get (void)=>int& do
    var int x=1;
    escape &x;
end
escape 10;
]],
    wrn = true,
    parser = 'line 1 : after `int´ : expected type modifier or `;´ or `do´',
    --env = 'line 3 : invalid escape value : local reference',
    --ref = 'line 3 : attribution to reference with greater scope',
}

Test { [[
vector[] byte str = [0,1,2];

code/instantaneous Fx (vector&[] byte vec)=>int do
    escape vec[1];
end

escape Fx(&str);
]],
    wrn = true,
    run = 1,
}
Test { [[
code/delayed Fx (event int e)=>void do
end
escape 0;
]],
    parser = 'line 1 : after `event´ : expected `&´',
}
Test { [[
code/delayed Fx (event& int e)=>void do
    await e;
end
escape 0;
]],
    wrn = true,
    run = 'TODO',
}
Test { [[
vector[] byte str = [0,1,2];

code/instantaneous Fx (vector&[] int vec)=>int do
    escape vec[1];
end

escape Fx(&str);
]],
    wrn = true,
    env = 'line 7 : wrong argument #1 : types mismatch (`int´ <= `byte´)',
}
Test { [[
vector[] byte str = [0,1,2];

code/instantaneous Fx (vector&[] byte vec)=>int do
    escape vec[1];
end

escape Fx(str);
]],
    wrn = true,
    ref = 'line 7 : invalid attribution : missing alias operator `&´',
}
Test { [[
vector[] byte str = [0,1,2];

code/instantaneous Fx (void) => byte[] do
    escape &this.str;
end

vector&[] byte ref = &Fx();

escape ref[1];
]],
    parser = 'line 3 : after `byte´ : expected type modifier or `;´ or `do´',
    --env = 'line 4 : invalid escape value : types mismatch (`byte[]´ <= `byte[]&´)',
}

-- vectors as argument (NO)
Test { [[
vector[] byte str = [0,1,2];

code/instantaneous Fx (var void&& x, vector[] int vec)=>int do
    escape vec[1];
end

escape Fx(str);
]],
    parser = 'line 3 : after `vector´ : expected `&´',
    --env = 'line 3 : wrong argument #2 : vectors are not supported',
    --env = 'line 7 : wrong argument #1 : types mismatch (`int[]´ <= `byte[]´)',
}

Test { [[
code/instantaneous Fx (var int a, var  void b)=>int do
end
escape 1;
]],
    wrn = true,
    env = 'line 1 : type cannot be `void´',
}

Test { [[
code/instantaneous Fx (var void, var int)=>int do
end
escape 1;
]],
    parser = 'line 1 : after `int´ : expected type modifier or `;´',
}

Test { [[
code/instantaneous Fx (var void a, var  int v)=>int do
end
escape 1;
]],
    wrn = true,
    env = 'line 1 : type cannot be `void´',
}

Test { [[
data Test with
    var& u8 b;
end

var u8 b = 7;
vector[3] Test v;
var Test t = Test(&b);
v = [] .. v .. [t];

// reassignments
b = 10;
t.b = 88;
v[0].b = 36; // invalid attribution : missing alias operator `&´

escape b;
]],
    run = 36,
}

Test { [[
var int x;
do
    x = 1;
end
escape x;
]],
    ref = 'line 1 : uninitialized variable "x" crossing compound statement (tests.lua:2)',
    --run = 1,
}

Test { [[
event void  a;
event& void b = &a;

par/or do
    await 1s;
    emit a;
with
    await b;
end

par/or do
    await 1s;
    emit b;
with
    await a;
end

escape 1;
]],
    run = { ['~>1s'] = 1 },
}

--<<< ALIASES / REFERENCES / REFS / &

Test { [[
native _f;
do _f(); finalize with nothing;
    end;
escape 1;
]],
    fin = 'line 2 : invalid `finalize´',
}

Test { [[
var int v = 0;
do
    do finalize with
        v = v * 2;
    end
    v = v + 1;
    do finalize with
        v = v + 3;
    end
end
escape v;
]],
    run = 8,
}

Test { [[
native _f;
native do void f (void* p) {} end

var void&& p=null;
do _f(p); finalize with nothing;
    end;
escape 1;
]],
    run = 1,
}

Test { [[
native _f;
native do void f () {} end

var void&& p = null;
do _f(p!=null); finalize with nothing;
    end;
escape 1;
]],
    fin = 'line 5 : invalid `finalize´',
    --run = 1,
}

Test { [[
native _f;
do
    var int&& p1 = null;
    do
        var int&& p2 = null;
        _f(p1, p2);
    end
end
escape 1;
]],
    fin = 'line 6 : invalid call (multiple scopes)',
}
Test { [[
native _enqueue, _V;
var byte&& buf = _V;
_enqueue(buf);
escape 1;
]],
    fin = 'line 2 : call requires `finalize´',
}

Test { [[
native _enqueue;
vector[255] byte buf;
_enqueue(buf);
escape 1;
]],
    env = 'line 2 : wrong argument #1 : cannot pass plain vectors to native calls',
    --fin = 'line 2 : call requires `finalize´',
}
Test { [[
native _enqueue;
vector[255] byte buf;
_enqueue(&&buf);
escape 1;
]],
    fin = 'line 2 : call requires `finalize´',
}

Test { [[
native _f;
do
    var int&& p1 = null;
    do
        var int&& p2 = null;
        _f(p1, p2);
    end
end
escape 1;
]],
    wrn = true,
    fin = 'line 6 : call requires `finalize´',
    -- multiple scopes
}

Test { [[
native _f;
native _v;
native do
    int v = 1;
    int f (int v) {
        escape v + 1;
    }
end
escape _f(_v);
]],
    --fin = 'line 3 : call requires `finalize´',
    run = 2,
    --fin = 'line 9 : attribution requires `finalize´',
}
Test { [[
native/pure _f;
native _v;
native do
    int v = 1;
    int f (int v) {
        escape v + 1;
    }
end
escape _f(_v);
]],
    --fin = 'line 3 : call requires `finalize´',
    run = 2,
}


Test { [[
native/pure _f;
native do
    int* f (int a) {
        escape NULL;
    }
end
var int&& v = _f(0);
escape v == null;
]],
    run = 1,
}

Test { [[
native/pure _f;
native do
    int V = 10;
    int f (int v) {
        escape v;
    }
end
native/const _V;
escape _f(_V);
]],
    run = 10;
}

Test { [[
native _f;
native do
    int f (int* v) {
        escape 1;
    }
end
var int v=0;
escape _f(&&v) == 1;
]],
    fin = 'line 8 : call requires `finalize´',
}

Test { [[
native/nohold _f;
native do
    int f (int* v) {
        escape 1;
    }
end
var int v=0;
escape _f(&&v) == 1;
]],
    run = 1,
}

Test { [[
native _V;
native/nohold _f;
native do
    int V=1;
    int f (int* v) {
        escape 1;
    }
end
var int v=0;
escape _f(&&v) == _V;
]],
    run = 1,
}

Test { [[
var int ret = 0;
var int&& pa=null;
do
    var int v=0;
    if v then end;
    if true then
        do finalize with
            ret = ret + 1;
    end
    else
        do finalize with
            ret = ret + 2;
    end
    end
end
escape ret;
]],
    run = 1,
}

Test { [[
var int ret = 0;
var int&& pa=null;
do
    var int v=0;
    if true then
        do pa = &&v;
        finalize with
            ret = ret + 1;
    end
    else
        do pa = &&v;
        finalize with
            ret = ret + 2;
    end
    end
end
escape ret;
]],
    --run = 1,
    fin = 'line 7 : attribution does not require `finalize´',
}
Test { [[
var int ret = 0;
var int&& pa=null;
do
    var int v=0;
    if true then
            pa = &&v;
    else
            pa = &&v;
    end
end
escape ret;
]],
    --run = 1,
    fin = 'line 6 : attribution to pointer with greater scope',
}

Test { [[
var int r = 0;
do
    await 1s;
    do finalize with
        do
            var int b = 1;
            r = b;
        end;
    end
end
escape r;
]],
    run = { ['~>1s']=1 },
}

Test { [[
var int ret = 0;
do
    await 1s;
    do finalize with
        var int a = 1;
    end
end
escape ret;
]],
    run = { ['~>1s']=0 },
}

Test { [[
do
    do finalize with
        if true then
        end;
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
var int ret=0;
do
    var int a = 1;
    do finalize with
        do
            a = a + 1;
            ret = a;
        end;
    end
end
escape ret;
]],
    run = 2,
}

Test { [[
var int ret=0;
do
    var int a = 1;
    do finalize with
        do
            a = a + 1;
            ret = a;
        end;
    end
end
escape ret;
]],
    run = 2,
}

Test { [[
var int ret = 0;
do
    var int a=0;
    do finalize with
        do
            a = 1;
            ret = a;
        end;
    end
end
escape ret;
]],
    run = 1,
}

Test { [[
var int a=0;
par/or do
    do finalize with
        a = 1;
    end
with
    a = 2;
end
escape a;
]],
    todo = '1 or 2: stack change',
    run = 1;
}

Test { [[
var int a=0;
par/or do
    do
        var int a;
        do finalize with
            a = 1;
    end
    end
with
    a = 2;
end
escape a;
]],
    todo = '1 or 2: stack change',
    run = 2;
}

Test { [[
var int ret=0;
par/or do
    do
        await 1s;
        do finalize with
            ret = 3;
    end
    end
with
    await 1s;
    ret = 2;
end
escape ret;
]],
    todo = '2 or 3: stack change',
    run = { ['~>1s']=3 },
}

Test { [[
input void A;
var int ret = 1;
loop do
    par/or do
        do
            await A;
            do finalize with
                ret = ret + 1;
            end
        end;
        escape 0;
    with
        break;
    end
end
escape ret;
]],
    wrn = true,
    run = 1,
}
Test { [[
input void A;
var int ret = 1;
loop do
    par/or do
        do
            await A;
            do finalize with
                ret = ret + 1;
            end
        end;
        escape 0;
    with
        break;
    end
end
escape ret;
]],
    wrn = true,
    run = 1,
}
Test { [[
input void A;
var int ret = 1;
loop do
    par/or do
        do
            await A;
            do finalize with
                ret = ret + 1;
            end
        end;
        escape 0;
    with
        break;
    end
end
escape ret;
]],
    ana = 'line 4 : at least one trail should terminate',
}

Test { [[
input void A;
var int ret = 1;
loop do
    par/or do
        do
            do finalize with
                ret = ret + 1;
    end
            await A;
        end;
        escape 0;
    with
        break;
    end
end
escape ret;
]],
    wrn = true,
    run = 2,
}

Test { [[
input void A, B;
var int ret = 1;
par/or do
    do
        do finalize with
            ret = 1;
    end
        await A;
    end
with
    do
        await B;
        do finalize with
            ret = 2;
    end
    end
end
escape ret;
]],
    run = {
        ['~>A']=1,
        ['~>B']=1
    },
}

Test { [[
input void A, B;
var int ret = 1;
par/or do
    do
        do finalize with
            ret = 1;
    end
        await A;
    end
with
    do
        await B;
        do finalize with
            ret = 2;
    end
    end
end
escape ret;
]],
    safety = 2,
    _ana = {
        acc = 1,
    },
    run = {
        ['~>A']=1,
        ['~>B']=1
    },
}

Test { [[
input void A, B, Z;
var int ret = 1;
par/or do
    do
        await A;
        do finalize with
            ret = 1;
    end
    end
with
    do
        await B;
        do finalize with
            ret = 2;
    end
    end
with
    do
        await Z;
        do finalize with
            ret = 3;
    end
    end
end
escape ret;
]],
    todo = 'finalizers do not run in parallel',
    _ana = {
        acc = 3,
    },
    run = { ['~>A']=0, ['~>B']=0, ['~>Z']=0 },
}

Test { [[
input void A, B;
event void a;
var int ret = 1;
par/or do
    do
        await A;
        do finalize with
            do
                emit a;
                ret = ret * 2;
    end
            end;
    end
with
    do
        await B;
        do finalize with
            ret = ret + 5;
    end
    end
with
    loop do
        await a;
        ret = ret + 1;
    end
end
escape ret;
]],
    props = 'line 9 : not permitted inside `finalize´',
}

Test { [[
input void A, B, Z;
event void a;
var int ret = 1;
par/or do
    do
        do finalize with
            ret = ret * 2;      // 7
    end
        await A;
        emit a;
    end
with
    do
        do finalize with
            ret = ret + 5;      // 15
    end
        await B;
    end
with
    loop do
        await a;
        ret = ret + 1;
    end
end
escape ret;
]],
    _ana = {
        acc = 3,
    },
    run = {
        ['~>A'] = 9,
        ['~>B'] = 12,
    },
}

Test { [[
input void A, B, Z;
event void a;
var int ret = 1;
par/or do
    do
        do finalize with
            ret = ret * 2;      // 7
    end
        await A;
        emit a;
    end
with
    do
        do finalize with
            ret = ret + 5;      // 15
    end
        await B;
    end
with
    loop do
        await a;
        ret = ret + 1;
    end
end
escape ret;
]],
    safety = 2,
    _ana = {
        acc = 9,
    },
    run = {
        ['~>A'] = 9,
        ['~>B'] = 12,
    },
}

Test { [[
input void A;
var int ret = 1;
par/or do
    do
        ret = ret + 1;
        do
            await A;
            do finalize with
                ret = ret * 3;
    end
        end
        do finalize with
            ret = ret + 5;
    end
    end
with
    await A;
    ret = ret * 2;
end
escape ret;
]],
    todo = 'ND: stack change',
    run = { ['~>A']=17 },
}

Test { [[
input void A;
var int ret = 1;
par/or do
    do
        ret = ret + 1;
        do
            await A;
            do finalize with
                ret = ret * 3;
            end
        end
        do finalize with
            ret = ret + 5;
        end
    end
with
    await A;
end
ret = ret * 2;
escape ret;
]],
    _ana = {
        abrt = 1,
    },
    run = { ['~>A']=22 },
}

Test { [[
input void A, B;
var int ret = 1;
par/or do
    do
        do finalize with
            ret = ret + 5;
        end
        ret = ret + 1;
        do
            do finalize with
                ret = ret * 3;
            end
            await A;
            ret = ret * 100;
        end
    end
with
    await B;
    ret = ret * 2;
end
escape ret;
]],
    run = { ['~>B']=17, ['~>A']=605 },
}

Test { [[
input void OS_START;
await OS_START;
par/or do
    await OS_START;
with
    await OS_START;
end
escape 1;
]],
    run = 0,
}

Test { [[
input void OS_START;
await OS_START;
do
    do finalize with
        var int ret = 1;
    end
    await OS_START;
end
escape 1;
]],
    run = 0,
}

Test { [[
input void A,B;
var int ret = 0;
loop do
    do
        do finalize with
            ret = ret + 4;
        end
        par/or do
            do
                do finalize with
                    ret = ret + 3;
                end
                await B;
                do
                    do finalize with
                        ret = ret + 2;
                    end
                    await B;
                    ret = ret + 1;
                end
            end
        with
            await A;
            break;
        end
    end
end
escape ret;
]],
    run = {
        ['~>A']         =  7,
        ['~>B;~>B;~>A'] = 17,
        ['~>B;~>A']     =  9,
    },
}

Test { [[
var int ret = 0;
loop do
    do
        ret = ret + 1;
        do break; end
        do finalize with
            ret = ret + 4;
    end
    end
end
escape ret;
]],
    wrn = true,
    run = 1,
}
Test { [[
var int ret = 0;
loop do
    do
        ret = ret + 1;
        do break; end
        do finalize with
            ret = ret + 4;
    end
    end
end
escape ret;
]],
     ana = 'line 6 : statement is not reachable',
}

Test { [[
var int ret = 0;
loop do
    do
        ret = ret + 1;
        do finalize with
            ret = ret + 4;
        end
        break;
    end
end
escape ret;
]],
    _ana = {
        unreachs = 2,
    },
    run = 5,
}

Test { [[
var int ret = 0;
loop do
    do
        await 1s;
        ret = ret + 1;
        do break; end
        do finalize with
            ret = ret + 4;
    end
    end
end
escape ret;
]],
    wrn = true,
    run = { ['~>1s']=1 },
}

Test { [[
var int ret = 0;
loop do
    do
        await 1s;
        ret = ret + 1;
        do finalize with
            ret = ret + 4;
    end
        break;
    end
end
escape ret;
]],
    _ana = {
        unreachs = 2,
    },
    run = { ['~>1s']=5 },
}

Test { [[
var int ret = do/_
    var int ret = 0;
    loop do
        do
            await 1s;
            ret = ret + 1;
            do escape ret * 2; end
            do finalize with
                ret = ret + 4;  // executed after `escape´ assigns to outer `ret´
    end
        end
    end
end;
escape ret;
]],
    _ana = {
        unreachs = 2,
    },
    run = { ['~>1s']=2 },
}

Test { [[
var int ret = do/_
    var int ret = 0;
    loop do
        do
            await 1s;
            ret = ret + 1;
            do finalize with
                ret = ret + 4;  // executed after `escape´ assigns to outer `ret´
    end
            escape ret * 2;
        end
    end
end;
escape ret;
]],
    _ana = {
        unreachs = 2,
    },
    run = { ['~>1s']=2 },
}

Test { [[
var int ret = 0;
par/or do
    await 1s;
with
    do
        await 1s;
        do finalize with
            ret = ret + 1;
    end
    end
end
escape ret;
]],
    _ana = {
        abrt = 1,
    },
    run = { ['~>1s']=0, },
}

Test { [[
var int ret = 10;
par/or do
    await 500ms;
with
    par/or do
        await 1s;
    with
        do
            do finalize with
                ret = ret + 1;
    end
            await 1s;
        end
    end
end
escape ret;
]],
    _ana = {
        unreachs = 4,  -- 1s,1s,or,fin
        abrt = 2,
    },
    run = { ['~>1s']=11, },
}

Test { [[
var int ret = 10;
par/or do
    await 500ms;
with
    par/or do
        await 1s;
    with
        do
            do finalize with
                ret = ret + 1;
    end
            await 250ms;
            ret = ret + 1;
        end
    end
end
escape ret;
]],
    _ana = {
        unreachs = 2,  -- 500ms,1s
        abrt = 2,
    },
    run = { ['~>1s']=12 },
}

Test { [[
input void A, B;
event void e;
var int v = 1;
par/or do
    do
        do finalize with
            do
                v = v + 1;
                v = v * 2;
            end;
    end
        await A;
        v = v + 3;
    end
with
    await e;
    v = v * 3;
with
    await B;
    v = v * 5;
end
escape v;
]],
    run = {
        ['~>B'] = 12,
        ['~>A'] = 10,
    }
}

Test { [[
pre native do
    void f (int* a) {
        *a = 10;
    }
    typedef void (*t)(int*);
end
native _t;
native/nohold _f;
var _t v = _f;
var int ret=0;
do
    var int a=0;
    do v(&&a);
        finalize with nothing; end;
    ret = a;
end
escape(ret);
]],
    run = 10,
}
Test { [[
native _t, _A;
native _f;
native do
    int* A = NULL;;
    void f (int* a) {
        A = a;
    }
    typedef void (*t)(int*);
end
var int ret = 0;
if _A then
    ret = ret + *(_A as int&&);
end
do
    var int a = 10;;
    var _t v = _f;
    do v(&&a);
        finalize with
            do
                ret = ret + a;
                _A = null;
        end
            end;
    if _A then
        a = a + *(_A as int&&);
    end
end
if _A then
    ret = ret + *(_A as int&&);
end
escape(ret);
]],
    run = 20,
}
Test { [[
input void OS_START;
native _t, _A;
native _f;
native do
    int* A = NULL;;
    void f (int* a) {
        A = a;
    }
    typedef void (*t)(int*);
end
var int ret = 0;
if _A then
    ret = ret + *(_A as int&&);
end
par/or do
        var int a = 10;;
        var _t v = _f;
        do v(&&a);
            finalize with
                do
                    ret = ret + a;
                    _A = null;
            end
                end;
        if _A then
            a = a + *(_A as int&&);
        end
        await FOREVER;
with
    await OS_START;
end
if _A then
    ret = ret + *(_A as int&&);
end
escape(ret);
]],
    --fin = 'line 32 : pointer access across `await´',
    run = 20,
}
Test { [[
var int v = 1;
par/or do
    nothing;
with
    v = *(null as int&&);
end
escape v;
]],
    run = 1,
}

Test { [[
do finalize with
end
escape 1;
]],
    run = 1,
}

Test { [[
native _f, _V;
native do
    int V;
    void f (int* x) {
        V = *x;
    }
end
var int ret = 10;
do
    var int x = 5;
    do _f(&&x); finalize with
        _V = _V + 1;
    end;
end
escape ret + _V;
]],
    run = 16,
}

Test { [[
event void&& e;
var void&& v = await e;
escape 1;
]],
    env = 'line 1 : invalid event type',
}

Test { [[
event int e;
var int v = await e;
escape 1;
]],
    run = 0,
}

Test { [[
event int e;
var int v = await e;
await e;
escape 1;
]],
    --fin = 'line 3 : cannot `await´ again on this block',
    run = 0,
}

Test { [[
input int&& E;
var int&& v = await E;
await E;
escape *v;
]],
    fin = 'line 4 : unsafe access to pointer "v" across `await´',
    --fin = 'line 3 : cannot `await´ again on this block',
    --run = 0,
}

Test { [[
var int&& p=null;
do
    input int&& E;
    p = await E;
end
escape 1;
]],
    run = 0,
    --fin = 'line 4 : invalid block for awoken pointer "p"',
}

Test { [[
var int&& p1=null;
do
    var int&& p;
    input int&& E;
    p = await E;
    p1 = p;
    await E;
    escape *p1;
end
escape 1;
]],
    --fin = 'line 6 : attribution requires `finalize´',
    --fin = 'line 8 : pointer access across `await´',
    fin = 'line 6 : attribution to pointer with greater scope',
}

Test { [[
native _f;
var int&& p1 = null;
do
    var int&& p;
    input int&& E;
    p = await E;
    _f(p);
    await E;
    escape *p1;
end
escape 1;
]],
    fin = 'line 6 : call requires `finalize´',
}

Test { [[
var int&& p=null;
do
    input int&& E;
    p = await E;
end
await 1s;
escape 1;
]],
    run = 0,
    --fin = 'line 4 : invalid block for pointer across `await´',
}

Test { [[
var int x = 10;
var int&& p = &&x;
par/or do
    await 1s;
with
    input int&& E;
    p = await E;
end
escape x;
]],
    --fin = 'line 8 : pointer access across `await´',
    --fin = 'line 6 : invalid block for pointer across `await´',
    --fin = 'line 6 : cannot `await´ again on this block',
    run = { ['~>1s']=10 },
}

Test { [[
input int&& A;
var int v=0;
par/or do
    do
        var int&& p = await A;
        v = *p;
    end
    await A;
with
    async do
        var int v = 10;
        emit A => &&v;
        emit A => null;
    end
end
escape v;
]],
    wrn = true,
    run = 10,
}

Test { [[
input int&& A;
var int v=0;
par/or do
    do
        var int&& p = await A;
        v = *p;
    end
    await A;
with
    async do
        var int v = 10;
        emit A => &&v;
        emit A => null;
    end
end
escape v;
]],
    wrn = true,
    run = 10,
    safety = 2,
    _ana = {
        acc = 1,
    },
}

Test { [[
input int&& A;
var int v=0;
par/or do
    do
        var int&& p = await A;
        v = *p;
    end
    await A;
with
    async do
        var int v = 10;
        emit A => (&&v as void&&);
        emit A => null;
    end
end
escape v;
]],
    env = 'line 12 : wrong argument #1',
    --wrn = true,
    --run = 10,
}

Test { [[
var int&& p=null;
var int ret=0;
input void OS_START;
do
    input int&& E;
    par/and do
        do p = await E;
        finalize with
            ret = *p;
            p = &&ret;
        end
    with
        await OS_START;
        var int i = 1;
        async (i) do
            emit E => &&i;
        end
    end
end
escape ret + *p;
]],
    adj = 'line 7 : invalid `finalize´',
    --fin = 'line 8 : attribution does not require `finalize´',
    --fin = 'line 8 : invalid block for awoken pointer "p"',
    --fin = 'line 14 : cannot `await´ again on this block',
}

Test { [[
var int ret = 0;
var int&& p = &&ret;
input void OS_START;
do
    input int&& E;
    par/and do
        p = await E;
    with
        await OS_START;
        var int i = 1;
        async (i) do
            emit E => &&i;
        end
    end
end
escape ret + *p;
]],
    run = 1,
    --env = 'line 11 : wrong argument : cannot pass pointers',
    fin = 'line 16 : unsafe access to pointer "p" across `async´ (tests.lua : 11)',
    --fin = 'line 14 : unsafe access to pointer "p" across `par/and´',
    --fin = 'line 8 : invalid block for awoken pointer "p"',
    --fin = 'line 14 : cannot `await´ again on this block',
}

Test { [[
native _assert;
var void&& p;
var int i;
input void OS_START;
do
    var int r;
    do
        input (int,void&&) PTR;
        par/or do
            do
                (i,p) = await PTR;
            finalize with
                r = i;
            end
        with
            await OS_START;
            async do
                emit PTR => (1, null);
            end
        end
    end
    _assert(r == 1);
    escape r;
end
]],
    --parser = 'line 10 : after `i´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `)´',
    --adj = 'line 9 : invalid `finalize´',
    --run = 1,
    -- TODO: impossible to place the finally in the correct parameter?
}

Test { [[
var int&& p = null;
var int ret = 0;
input void OS_START;
do
    input int&& E;
    par/and do
        p = await E;
        ret = *p;
    with
        await OS_START;
        async do
            var int i = 1;
            emit E => &&i;
        end
    end
end
escape ret;
]],
    --env = 'line 12 : wrong argument : cannot pass pointers',
    --fin = 'line 7 : invalid block for awoken pointer "p"',
    --fin = 'line 7 : wrong operator',
    run = 1,
}

Test { [[
var int&& p = null;
var int ret = 0;
input void OS_START;
do
    input int&& E;
    par/and do
        p = await E;
        ret = *p;
    with
        await OS_START;
        async do
            var int i = 1;
            emit E => &&i;
        end
    end
end
escape ret;
]],
    --env = 'line 12 : wrong argument : cannot pass pointers',
    --fin = 'line 7 : invalid block for awoken pointer "p"',
    --fin = 'line 7 : wrong operator',
    --run = 1,
    safety = 2,
    _ana = {
        acc = 1,
    },
}

Test { [[
input void OS_START;
var int ret=0;
event (bool,int) ok;
par/or do
    await OS_START;
    emit ok => (true,10);
with
    var bool b;
    (b,ret) = await ok;
    if b then end;
end
escape ret;
]],
    run = 10,
}

Test { [[
input void OS_START;
input (int,void&&) PTR;
var void&& p=null;
var int i=0;
par/or do
    (i,p) = await PTR;
with
    await OS_START;
    async do
        emit PTR => (1, null);
    end
end
escape i;
]],
    --env = 'line 9 : wrong argument #2 : cannot pass pointers',
    --fin = 'line 6 : invalid block for awoken pointer "p"',
    --fin = 'line 6 : attribution to pointer with greater scope',
    run = 1,
}
Test { [[
input void OS_START;
input (int,void&&) PTR;
var void&& p=null;
var int i=0;
par/or do
    var void&& p1;
    (i,p1) = await PTR;
    p := p1;
with
    await OS_START;
    async do
        emit PTR => (1, null);
    end
end
escape i;
]],
    --env = 'line 11 : wrong argument #2 : cannot pass pointers',
    --fin = 'line 6 : invalid block for awoken pointer "p"',
    run = 1,
}

Test { [[
input (int,void&&) PTR;
var void&& p;
var int i;
(i,p) = await PTR;
await 1s;
escape i;
]],
    --fin = 'line 5 : cannot `await´ again on this block',
    run = 0,
}

Test { [[
input void OS_START;
input (int,void&&) PTR;
var void&& p = null;
var int i = 0;
par/or do
    var void&& p1;
    (i,p1) = await PTR;
    p := p1;
with
    await OS_START;
    async do
        emit PTR => (1, null);
    end
end
await 1s;
escape i;
]],
    --env = 'line 11 : wrong argument #2 : cannot pass pointers',
    run = 0,
    --fin = 'line 6 : invalid block for awoken pointer "p"',
}

Test { [[
var void&& p = null;
var int i = 0;
input void OS_START;
do
    input (int,void&&) PTR;
    par/or do
        (i,p) = await PTR;
    with
        await OS_START;
        async do
            emit PTR => (1, null);
        end
    end
end
escape i;
]],
    --env = 'line 10 : wrong argument #2 : cannot pass pointers',
    --fin = 'line 7 : wrong operator',
    --fin = 'line 7 : attribution does not require `finalize´',
    run = 1,
}

Test { [[
var void&& p = null;
var int i = 0;
input void OS_START;
do
    input (int,void&&) PTR;
    par/or do
        var void&& p1;
        (i,p1) = await PTR;
        p := p1;
    with
        await OS_START;
        async do
            emit PTR => (1, null);
        end
    end
end
escape i;
]],
    --fin = 'line 7 : wrong operator',
    --fin = 'line 7 : attribution does not require `finalize´',
    --env = 'line 12 : wrong argument #2 : cannot pass pointers',
    run = 1,
}

Test { [[
var int&& p = null;
var int i = 0;
input void OS_START;
do
    input (int,int&&) PTR;
    par/or do
        var int&& p1;
        (i,p1) = await PTR;
        p := p1;
    with
        await OS_START;
        async do
            var int v = 10;
            emit PTR => (1, &&v);
        end
    end
    i = *p;
end
escape i;
]],
    --fin = 'line 7 : wrong operator',
    --fin = 'line 7 : attribution does not require `finalize´',
    --fin = 'line 14 : pointer access across `await´',
    --env = 'line 13 : wrong argument #2 : cannot pass pointers',
    --run = 10,
    fin = 'line 17 : unsafe access to pointer "p" across `async´ (tests.lua : 12)',
}

Test { [[
input (int,int,int&&) A;
async do
    emit A =>
        (1, 1, null);
end
escape 1;
]],
    run = 1;
}

Test { [[
native _ptr, _malloc;
native do
    void&& ptr;
end
_ptr = _malloc(1);
escape 1;
]],
    fin = 'line 4 : attribution requires `finalize´',
}
Test { [[
native _ptr, _malloc;
native/nohold _free;
native do
    void* ptr;
end
do _ptr = _malloc(100);
finalize with
    _free(_ptr);
end
escape 1;
]],
    run = 1,
}

Test { [[
native _alloc;
native do
    int V;
    int* alloc (int ok) {
        escape &V;
    }
    void dealloc (int* ptr) {
    }
end
native/nohold _dealloc;

var& int? tex;
do tex = &_alloc(1);    // v=2
finalize with
    _dealloc(&&tex);
end

escape 1;
]],
    env = 'line 15 : invalid operand to unary "&&" : option type',
}

Test { [[
native _alloc;
native do
    int V;
    int* alloc (int ok) {
        escape &V;
    }
    void dealloc (int* ptr) {
    }
end
native/nohold _dealloc;

var& int? tex;
do tex = &_alloc(1);    // v=2
finalize with
    _dealloc(&&tex!);
end

escape 1;
]],
    run = 1,
}

Test { [[
native _alloc;
native do
    int* alloc (int ok) {
        escape NULL;
    }
    void dealloc (int* ptr) {
    }
end
native/nohold _dealloc;

var& int? tex;
do
    tex = &_alloc(1);    // v=2
finalize
with
    _dealloc(&&tex!);
end

escape 1;
]],
    asr = true,
}

Test { [[
native _alloc, _V;
native do
    int* alloc (int ok) {
        escape NULL;
    }
    int V = 0;
    void dealloc (int* ptr) {
        if (ptr == NULL) {
            V = 1;
        }
    }
end
native/nohold _dealloc;

do
    var& int? tex;
do tex = &_alloc(1);
    finalize with
        _dealloc(tex);
    end
end

escape _V;
]],
    env = 'line 19 : wrong argument #1 : cannot pass option values to native calls',
    --run = 1,
}

Test { [[
native _alloc, _V;
native do
    int* alloc (int ok) {
        escape NULL;
    }
    int V = 0;
    void dealloc (int* ptr) {
        if (ptr == NULL) {
            V = 1;
        }
    }
end
native/nohold _dealloc;

do
    var& int? tex;
    do tex = &_alloc(1);
    finalize with
        _dealloc(&tex!);
    end
end

escape _V;
]],
    env = 'line 19 : wrong argument #1 : cannot pass aliases to native calls',
    --run = '19] runtime error: invalid tag',
}

Test { [[
native do
    int* alloc (int ok) {
        escape NULL;
    }
    int V = 0;
    void dealloc (int* ptr) {
        if (ptr == NULL) {
            V = 1;
        }
    }
end
native _alloc, _V;
native/nohold _dealloc;

do
    var& int? tex;
    do tex = &_alloc(1);
    finalize with
        _dealloc(&&tex!);
    end
end

escape _V;
]],
    --env = 'line 19 : wrong argument #1 : cannot pass option type',
    run = '19] runtime error: invalid tag',
}

Test { [[
pre native do
    struct Tx;
    typedef struct Tx t;
    int V = 1;
    t* alloc (int ok) {
        if (ok) {
            V++;
            escape (t*) &V;
        } else {
            escape NULL;
        }
    }
    void dealloc (t* ptr) {
        if (ptr != NULL) {
            V *= 2;
        }
    }
end
native _alloc, _V, _t;
native/nohold _dealloc;

var int ret = _V;           // v=1, ret=1

do
    var& _t? tex;
do
        tex = &_alloc(1);    // v=2
    finalize
    with
        _dealloc(&&tex!);
    end
    ret = ret + _V;         // ret=3
    if not tex? then
        ret = 0;
    end
end                         // v=4

ret = ret + _V;             // ret=7

do
    var& _t? tex;
do
        tex = &_alloc(0);    // v=4
    finalize
    with
        if tex? then
            _dealloc(&&tex!);
        end
    end
    ret = ret + _V;         // ret=11
    if not tex? then
        ret = ret + 1;      // ret=12
    end
end                         // v=4

ret = ret + _V;             // ret=16

escape ret;
]],
    run = 16,
}

Test { [[
native _f;
native do
    void* f () {
        escape NULL;
    }
end

var& void? ptr;
do ptr = &_f();
finalize with
    nothing;
end

escape &ptr! == &ptr!;  // ptr.SOME fails
]],
    env = 'line 14 : invalid use of operator "&" : not a binding assignment',
}

Test { [[
native _f;
native do
    void* f () {
        escape NULL;
    }
end

var& void? ptr;
do
    ptr = &_f();
finalize
with
    nothing;
end

escape &&ptr! == &&ptr!;  // ptr.SOME fails
]],
    asr = true,
}

Test { [[
native _f;
native do
    void* f () {
        escape NULL;
    }
end

var& void? ptr;
do
    ptr = &_f();
finalize
with
    nothing;
end

escape not ptr?;
]],
    run = 1,
}

Test { [[
native _f;
native do
    void* f () {
        escape NULL;
    }
    void g (void* g) {
    }
end
native/nohold _g;

var& void? ptr;
do
    ptr = &_f();
finalize
with
    _g(&&ptr!);    // error (ptr is Nil)
end

escape not ptr?;
]],
    asr = true
}

Test { [[
native _f;
native do
    void* f () {
        escape NULL;
    }
    void g (void* g) {
    }
end
native/nohold _g;

var int ret = 0;

do
    var& void? ptr;
do
        ptr = &_f();
    finalize
    with
        if ptr? then
            _g(&&ptr!);
        else
            ret = ret + 1;
        end
    end
    ret = ret + (not ptr?);
end

escape ret;
]],
    run = 2,
}

Test { [[
native _alloc, _V;
native do
    int V = 1;
    int* alloc () {
        escape &V;
    }
end

var& int? tex1;
do tex1 = &_alloc(1);
finalize with
    nothing;
end

var& int tex2 = tex1;

escape &tex2==&_V;
]],
    env = 'line 15 : types mismatch (`int&´ <= `int&?´)',
    --run = 1,
}

Test { [[
native _alloc, _V;
native do
    int V = 1;
    int* alloc () {
        escape NULL;
    }
end

var& int? tex1;
do tex1 = &_alloc(1);
finalize with
    nothing;
end

var& int tex2 = tex1;

escape &tex2==&_V;
]],
    env = 'line 15 : types mismatch (`int&´ <= `int&?´)',
    --asr = true,
}

Test { [[
native _V, _t, _alloc;
pre native do
    struct Tx;
    typedef struct Tx t;
    int V = 1;
    t* alloc (int ok) {
        if (ok) {
            V++;
            escape (t*) &V;
        } else {
            escape NULL;
        }
    }
    void dealloc (t* ptr) {
        if (ptr != NULL) {
            V *= 2;
        }
    }
end
native/nohold _dealloc;

var int ret = _V;           // v=1, ret=1

do
    var& _t? tex;
do
        tex = &_alloc(1);    // v=2
    finalize
    with
        _dealloc(&&tex!);
    end
    ret = ret + _V;         // ret=3
    if not tex? then
        ret = 0;
    end
end                         // v=4

ret = ret + _V;             // ret=7

do
    var& _t? tex;
do
        tex = &_alloc(0);    // v=4
    finalize
    with
        if tex? then
            _dealloc(&&tex!);
        end
    end
    ret = ret + _V;         // ret=11
    if not tex? then
        ret = ret + 1;      // ret=12
    end
end                         // v=4

ret = ret + _V;             // ret=16

escape ret;
]],
    run = 16,
}

Test { [[
native _SDL_Window, _SDL_CreateWindow, _SDL_WINDOW_SHOWN;
native/nohold _SDL_DestroyWindow;


var& _SDL_Window win;
do win = &_SDL_CreateWindow("UI - Texture",
                            500, 1300, 800, 480, _SDL_WINDOW_SHOWN);
    finalize with
        _SDL_DestroyWindow(win);
    end
escape 0;
]],
    fin = 'line 6 : must assign to a option reference (declared with `&?´)',
}

Test { [[
native _V, _my_alloc, _my_free;
native do
    int V = 0;
    void* my_alloc (void) {
        V += 1;
        escape NULL;
    }
    void my_free () {
        V *= 2;
    }
end

input void SDL_REDRAW;

par/or do
    await 1s;
    _V = _V + 100;
with
    every SDL_REDRAW do
        var& void? srf;
do
            srf = &_my_alloc();
        finalize
        with
            if srf? then end;
            _my_free();
        end
    end
end
escape _V;
]],
    run = { ['~>SDL_REDRAW;~>SDL_REDRAW;~>SDL_REDRAW;~>1s']=114 },
}

Test { [[
loop do
    do finalize with
        break;
    end
end
escape 1;
]],
    props = 'line 3 : not permitted inside `finalize´',
}

Test { [[
do finalize with
    escape 1;
end
escape 1;
]],
    props = 'line 2 : not permitted inside `finalize´',
}

Test { [[
do finalize with
    loop do
        if true then
            break;
        end
    end
end
escape 1;
]],
    tight = 'line 2 : tight loop',
    run = 1,
}

Test { [[
do finalize with
    var int ok = do/_
        escape 1;
    end;
end
escape 1;
]],
    run = 1,
}

    -- ASYNCHRONOUS

Test { [[
input void A;
var int ret=0;
var& int pret = &ret;
par/or do
   async(pret) do
      pret=10;
    end;
with
   await A;
   ret = 1;
end
escape ret;
]],
    run = { ['~>A']=10 },
}

Test { [[
input void A;
var int ret=0;
var& int pret = &ret;
par/or do
   async(pret) do
      pret=10;
    end;
with
   await A;
   ret = 1;
end
escape ret;
]],
    run = { ['~>A']=10 },
    safety = 2,
    _ana = {
        acc = 1,
    },
}

Test { [[
async do
    escape 1;
end;
escape 0;
]],
    --props = 'line 2 : not permitted inside `async´',
    props = 'line 2 : not permitted across `async´ declaration',
}

Test { [[
var int a = async do
    escape 1;
end;
escape a;
]],
    parser = 'line 1 : after `=´ : expected expression',
}

Test { [[
var int a,b;
async (b) do
    a = 1;
end;
escape a;
]],
    locs = 'line 3 : internal identifier "a" is not declared',
    --run = 1,
}

Test { [[
var int a;
async do
    a = 1;
end;
escape a;
]],
    locs = 'line 3 : internal identifier "a" is not declared',
    --run = 1,
}

Test { [[
par/and do
    async do
        escape 1;
    end;
with
    escape 2;
end;
]],
    props = 'line 3 : not permitted across `async´ declaration',
    --props = 'line 3 : not permitted inside `async´',
}

Test { [[
par/and do
    async do
    end;
    escape 1;
with
    escape 2;
end;
]],
    --abrt = 1,
    run = 2,
    _ana = {
        unreachs = 3,
    },
}

Test { [[
par/and do
    async do
    end;
    escape 1;
with
    escape 2;
end;
]],
    --abrt = 1,
    run = 2,
    safety = 2,
    _ana = {
        acc = 1,
        unreachs = 3,
    },
}

Test { [[
var int a;
par/and do
    async do
        a = 1;
    end;
with
    a = 2;
end;
escape a;
]],
    locs = 'line 4 : internal identifier "a" is not declared',
    _ana = {
        --acc = 1,
    },
}

Test { [[
async do
    escape 1+2;
end;
]],
    --props = 'line 2 : not permitted inside `async´',
    props = 'line 2 : not permitted across `async´ declaration',
}

Test { [[
var int a = 1;
var& int pa = &a;
async (a) do
    var int a = do/_
        escape 1;
    end;
    escape a;
end;
escape a;
]],
    wrn = true,
    props = 'line 7 : not permitted across `async´ declaration',
    --props = 'line 5 : not permitted inside `async´',
}

Test { [[
input void X;
async do
    emit X;
end;
escape 0;
]],
    run=0
}

Test { [[
input int A;
var int a;
async do
    a = 1;
    emit A => a;
end;
escape a;
]],
    locs = 'line 4 : internal identifier "a" is not declared',
    --run=1
}

Test { [[
input void A;
var int a;
async do
    a = emit A;
end;
escape a;
]],
    --env = "line 4 : invalid attribution",
    locs = 'line 4 : internal identifier "a" is not declared',
    --parser = 'line 4 : after `=´ : expected expression',
}

Test { [[
event int a;
async do
    emit a => 1;
end;
escape 0;
]],
    locs = 'line 3 : internal identifier "a" is not declared',
}
Test { [[
event int a;
async do
    await a;
end;
escape 0;
]],
    locs = 'line 3 : internal identifier "a" is not declared',
}
Test { [[
async do
    await 1ms;
end;
escape 0;
]],
    props='not permitted inside `async´'
}
Test { [[
input int X;
async do
    emit X => 1;
end;
emit X => 1;
escape 0;
]],
  props='invalid `emit´'
}
Test { [[
async do
    async do
    end;
end;
]],
    props='not permitted inside `async´'
}
Test { [[
async do
    par/or do
    with
    end;
end;
]],
    props='not permitted inside `async´'
}

Test { [[
loop do
    async do
        break;
    end;
end;
escape 0;
]],
    props='`break´ without loop'
}

Test { [[
native _a;
native do
    int a;
end
async do
    _a = 1;
end
escape _a;
]],
    run = 1,
}

Test { [[
native _a;
native do
    int a, b;
end
par/and do
    async do
        _a = 1;
    end
with
    async do
        _a = 1;
    end
end
escape _a+_b;
]],
    todo = 'async is not simulated',
    _ana = {
        acc = 1,
    },
}

Test { [[
@const _a;
deterministic _b with _c;

native do
    int a = 1;
    int b;
    int c;
end
par/and do
    async do
        _b = 1;
    end
with
    async do
        _a = 1;
    end
with
    _c = 1;
end
escape _a+_b+_c;
]],
    todo = true,
    run = 3,
}

Test { [[
native _a,_b;
native do
    int a=1,b=1;
end
par/or do
    _a = 1;
with
    _b = 1;
end
escape _a + _b;
]],
    _ana = {
        abrt = 1,
    },
    run = 2,
}

Test { [[
native _a,_b;
native do
    int a = 1;
end
var int a=0;
par/or do
    _a = 1;
with
    a = 1;
end
escape _a + a;
]],
    _ana = {
        abrt = 1,
    },
    run = 1,
}

Test { [[
native _a;
native do
    int a = 1;
end
var int a=0;
deterministic a with _a;
par/or do
    _a = 1;
with
    a = 1;
end
escape _a + a;
]],
    _ana = {
        abrt = 1,
    },
    run = 1,
}

Test { [[
native do
    int a = 1;
    int b;
    int c;
end
par/and do
    async do
        _b = 1;
    end
with
    async do
        _a = 1;
    end
with
    _c = 1;
end
escape _a+_b+_c;
]],
    todo = 'nd in async',
    _ana = {
        acc = 3,
    },
}

Test { [[
var int r=0;
async(r) do
    var int i = 100;
    r = i;
end;
escape r;
]],
    run=100
}

Test { [[
var int ret=0;
async (ret) do
    var int i = 100;
    var int sum = 10;
    sum = sum + i;
    ret = sum;
end;
escape ret;
]],
    run = 110,
}

-- sync version
Test { [[
input int B;
var int ret = 0;
var int f = 0;
par/or do
    ret = do/_
        var int sum = 0;
        var int i = 0;
        loop do
            sum = sum + i;
            if i == 100 then
                break;
            else
                await 1ms;
                i = i + 1;
            end
        end
        escape sum;
    end;
with
    f = await B;
end;
escape ret+f;
]],
    run = {
        ['10~>B'] = 10,
        ['~>1s'] = 5050,
    }
}

Test { [[
input int B;
var int ret = 0;
var int f=0;
par/and do
    async(ret) do
        var int sum = 0;
        var int i = 0;
        loop do
            sum = sum + i;
            if i == 100 then
                break;
            else
                i = i + 1;
            end
        end
        ret = sum;
    end;
with
    f = await B;
end;
escape ret+f;
]],
    run = { ['10~>B']=5060 }
}

Test { [[
input int B;
var int ret = 0;
var int f=0;
par/and do
    async(ret) do
        var int sum = 0;
        var int i = 0;
        loop do
            sum = sum + i;
            if i == 100 then
                break;
            else
                i = i + 1;
            end
        end
        ret = sum;
    end;
with
    f = await B;
end;
escape ret+f;
]],
    run = { ['10~>B']=5060 },
    safety = 2,
}

Test { [[
input int B;
var int ret = 0;
var int f=0;
par/or do
    async(ret) do
        var int sum = 0;
        var int i = 0;
        loop do
            sum = sum + i;
            if i == 100 then
                break;
            else
                i = i + 1;
            end
        end
        ret =  sum;
    end;
with
    f = await B;
end;
escape ret+f;
]],
    run = { ['10~>B']=10 }
}

Test { [[
input int B;
par do
    await B;
    escape 1;
with
    async do
        loop do
            if false then
                break;
            end;
        end;
    end;
    escape 0;
end;
]],
    run = { ['1~>B'] = 1 },
}

Test { [[
input int B;
par/or do
    await B;
with
    async do
        loop do
        end;
    end;
end;
escape 0;
]],
    todo = 'detect termination',
    props='async must terminate'
}

Test { [[
var int ret=0;
async (ret) do
    var int i = 100;
    i = i - 1;
    ret = i;
end;
escape ret;
]],
    run = 99,
}

Test { [[
var int ret=0;
async(ret) do
    var int i = 100;
    loop do
        break;
    end;
    ret = i;
end;
escape ret;
]],
    _ana = {
        --unreachs = 1,       -- TODO: loop iter
    },
    run = 100,
}

Test { [[
var int ret=0;
async(ret) do
    var int i = 0;
    if i then
        i = 1;
    else
        i = 2;
    end
    ret = i;
end;
escape ret;
]],
    run = 2,
}

Test { [[
var int i=0;
var& int pi=&i;
async (pi) do
    var int i = 10;
    loop do
        i = i - 1;
        if not i then
            pi = i;
            break;
        end;
    end;
end;
escape i;
]],
    run = 0,
    wrn = true,
}

Test { [[
var int i=0;
var& int pi = &i;
async (pi) do
    var int i = 10;
    loop do
        i = i - 1;
        if not i then
            pi = i;
            break;
        end;
    end;
end;
escape i;
]],
    run = 0,
    wrn = true,
}


Test { [[
var int i = async do
    var int i = 10;
    loop do
        i = i - 1;
    end;
    escape 0;
end;
escape i;
]],
    _ana = {
        unreachs = 3,
        isForever = false,
    },
    --dfa = true,
    todo = true,    -- no simulation for async
}

Test { [[
var int i = 10;
var& int pi = &i;
async (pi) do
    loop do
        i = i - 1;
        if not i then
            pi = i;
            break;
        end;
    end;
end;
escape i;
]],
    locs = 'line 5 : internal identifier "i" is not declared',
}

Test { [[
var int sum=0;
var& int p = &sum;
async (p) do
    var int i = 10;
    var int sum = 0;
    loop do
        sum = sum + i;
        i = i - 1;
        if not i then
            p = sum;
            break;
        end;
    end;
end;
escape sum;
]],
    wrn = true,
    run = 55,
}

Test { [[
input int A;
par do
    async do
        emit A => 1;
    end;
    escape 0;
with
    await A;
    escape 5;
end;
]],
    run = 5,
}

Test { [[
input int A, B;
var int a = 0;
par/or do
    async do
    end;
with
    await B;
end;
a = a + 1;
await A;
escape a;
]],
    run = {
        ['1~>B ; 10~>A'] = 1,
    },
}

Test { [[
input int A;
par/or do
    async do
        emit A => 4;
    end;
with
end;
escape 1;
]],
    _ana = {
        unreachs = 1,
    },
    run = 1,
}

-- round-robin test
Test { [[
input void A;
var int ret = 0;
par/or do
    loop do
        async do
            emit A;
        end
        ret = ret + 1;
    end
with
    par/and do
        var int v = async do
            var int v;
            loop i in [0 |> 5[ do
                v = v + i;
            end
            escape v;
        end;
        ret = ret + v;
    with
        var int v = async do
            var int v;
            loop i in [0 |> 5[ do
                v = v + i;
            end
            escape v;
        end;
        ret = ret + v;
    end
end
escape ret;
]],
    todo = 'algo now is nondet',
    _ana = {
        --unreachs = 1,       -- TODO: async
    },
    run = 23,
}

Test { [[
native _tceu_queue;
input void&& E;
input _tceu_queue&& GO;
every qu_ in GO do
    var _tceu_queue qu = * qu_;
    async(qu) do
        emit E => qu.param.ptr;
    end
end
]],
    fin = 'line 5 : unsafe access to pointer "qu" across `async´',
    --_ana = { isForever=true },
    --run = 1,
}

Test { [[
input void&& E;
native/plain _tceu_queue;
input _tceu_queue&& GO;
every qu_ in GO do
    var _tceu_queue qu = * qu_;
    async(qu) do
        emit E => qu.param.ptr;
    end
end
]],
    _ana = {
        isForever = true,
    },
}

-- HIDDEN
Test { [[
var int a = 1;
var int&& b = &&a;
do
var int a = 0;
if a then end;
end
escape *b;
]],
    wrn = true,
    run = 1,
}

-- INPUT / OUTPUT / CALL

Test { [[
input void A;
input void A;
escape 1;
]],
    tops = 'line 2 : identifier "A" is already declared (tests.lua : line 1)',
}

Test { [[
input void A;
input int A;
escape 1;
]],
    tops = 'line 2 : identifier "A" is already declared (tests.lua : line 1)',
}

--if not OS then

Test { [[
output xxx A;
escape(1);
]],
    parser = "line 1 : after `output´ : expected type",
}
Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing(d)
end
output int A;
emit A => 1;
escape(1);
]],
    run=1
}
Test { [[
output int A;
if emit A => 1 then
    escape 0;
end
escape(1);
]],
    parser = 'line 2 : after `if´ : expected expression',
}
Test { [[
native do
    #define ceu_out_emit(a,b,c,d) 1
end
output int A;
if emit A => 1 then
    escape 0;
end
escape(1);
]],
    parser = 'line 5 : after `if´ : expected expression',
}

Test { [[
output t A;
emit A => 1;
escape(1);
]],
    parser = 'line 1 : after `output´ : expected type',
}
Test { [[
output t A;
emit A => 1;
escape(1);
]],
    parser = 'line 1 : after `output´ : expected type',
}
Test { [[
native _t;
output _t&& A;
emit A => 1;
escape(1);
]],
    env = 'line 2 : wrong argument #1',
}
Test { [[
native _t;
output int A;
var _t v=1;
emit A => v;
escape(1);
]],
    --env = 'line 2 : undeclared type `_t´',
    --env = 'line 3 : non-matching types on `emit´',
    gcc = 'error: unknown type name',
}
Test { [[
native _t;
native do
    #define ceu_out_emit(a,b,c,d) __ceu_nothing(d)
end
output int A;
pre native do
    typedef int t;
end
var _t v=1;
emit A => v;
escape(1);
]],
    --env = 'line 2 : undeclared type `_t´',
    --env = 'line 3 : non-matching types on `emit´',
    run = 1,
}
Test { [[
output int A;
var int a;
emit A => &&a;
escape(1);
]],
    env = 'line 3 : wrong argument #1',
}
Test { [[
output int A;
var int a;
if emit A => &&a then
    escape 0;
end
escape(1);
]],
    parser = 'line 3 : after `if´ : expected expression',
    --env = 'line 3 : non-matching types on `emit´',
}
Test { [[
native _char;
output _char A;
escape 1;
]],
    wrn = true,
    run = 1,
    --env = "line 1 : invalid event type",
}

Test { [[
native do
    /******/
    int end = 1;
    /******/
end
native _end;
escape _end;
]],
    run = 1
}

Test { [[
pre native do
    typedef struct {
        int a;
        int b;
    } t;
end
native _t;
var _t v;
v.a = 1;
v.b = 2;
escape v.a + v.b;
]],
    ref = 'line 8 : invalid access to uninitialized variable "v" (declared at tests.lua:7)',
}
Test { [[
pre native do
    typedef struct {
        int a;
        int b;
    } t;
end
native/plain _t;
var _t v = _t(1,2);
escape v.a + v.b;
]],
    run = 3,
}

Test { [[
pre native do
    ##include <assert.h>
    typedef struct {
        int a;
        int b;
    } t;
end
native do
    ##define ceu_out_emit(a,b,c,d) Fa(a,b,c,d)
    int Fa (tceu_app* app, int evt, int sz, void* v) {
        if (evt == CEU_OUT_A) {
            t* x = ((tceu__t_*)v)->_1;
            escape x->a + x->b;
        } else {
            escape *((int*)v);
        }
    }
end
native/plain _t;
output _t&& A;
output int B;
var int a, b;

var _t v = _t(1,-1);
a = emit A => &&v;
b = emit B => 5;
escape a + b;
]],
    run = 5,
    --parser = 'line 26 : after `=´ : expected expression',
}

Test { [[
pre native do
    ##include <assert.h>
    typedef struct {
        int a;
        int b;
    } t;
end
native do
    ##define ceu_out_emit(a,b,c,d) Fa(a,b,c,d)
    int Fa (tceu_app* app, int evt, int sz, void* v) {
        if (evt == CEU_OUT_A) {
            t x = ((tceu__t*)v)->_1;
            escape x.a + x.b;
        } else {
            escape *((int*)v);
        }
    }
end
native/plain _t;
output _t A;
output int B;
var int a, b;

var _t v = _t(1,-1);
a = emit A => v;
b = emit B => 5;
escape a + b;
]],
    run = 5,
    --parser = 'line 26 : after `=´ : expected expression',
}

Test { [[
native _cahr;
output void A;
native do
    void A (int v) {}
end
var _cahr v = emit A => 1;
escape 0;
]],
    env = 'line 6 : arity mismatch',
    --env = 'line 6 : non-matching types on `emit´',
    --parser = 'line 6 : after `=´ : expected expression',
    --env = 'line 6 : undeclared type `_cahr´',
}
Test { [[
native _char;
output void A;
var _char v = emit A => ;
escape v;
]],
    parser = 'line 3 : after `=>´ : expected expression',
    --parser = 'line 3 : before `=>´ : expected `;´',
    --env = 'line 3 : invalid attribution',
}
Test { [[
output void A;
native do
    void A (int v) {}
end
native _char;
var _char v = emit A => 1;
escape 0;
]],
    --parser = 'line 6 : after `=´ : expected expression',
    --env = 'line 6 : non-matching types on `emit´',
    env = 'line 6 : arity mismatch',
}

Test { [[
native do
    void A (int v) {}
end
emit A => 1;
escape 0;
]],
    tops = 'external "A" is not declared',
}

Test { [[
native do
    #define ceu_out_emit(a,b,c,d)  __ceu_nothing(d)
end
output void A, B;
par/or do
    emit A;
with
    emit B;
end
escape 1;
]],
    _ana = {
        acc = 1,
        abrt = 3,
    },
    run = 1,
}

Test { [[
native do
    #define ceu_out_emit(a,b,c,d)  __ceu_nothing(d)
end
deterministic A with B;
output void A, B;
par/or do
    emit A;
with
    emit B;
end
escape 1;
]],
    tops = 'line 4 : external "A" is not declared',
}

Test { [[
native do
    #define ceu_out_emit(a,b,c,d)  __ceu_nothing(d)
end
output void A, B;
deterministic A with B;
par/or do
    emit A;
with
    emit B;
end
escape 1;
]],
    _ana = {
        abrt = 3,
    },
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) Fx(d)
    void Fx (void* p) {
        tceu__int___int_* v = (tceu__int___int_*) p;
        *(v->_1) = 1;
        *(v->_2) = 2;
    }
end

output (int&&,  int&&) RADIO_SEND;
var int a=1,b=1;
emit RADIO_SEND => (&&a,&&b);

escape a + b;
]],
    run = 3,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) Fx(a,b,d)
    void Fx (tceu_app* app, int evt, void* p) {
        tceu__int___int_* v = (tceu__int___int_*) p;
        *(v->_1) = (evt == CEU_OUT_RADIO_SEND);
        *(v->_2) = 2;
    }
end

output (int&&,  int&&) RADIO_SEND;
var int a=1,b=1;
emit RADIO_SEND => (&&a,&&b);

escape a + b;
]],
    run = 3,
}

Test { [[
native _Fx;
output int Z;
native do
    void Z() {};
end
par do
    _Fx();
with
    emit Z => 1;
end
]],
    _ana = {
        reachs = 1,
        acc = 1,
        isForever = true,
    },
}

Test { [[
native _Fx;
output int Z,W;
native do
    void Z() {};
end
par do
    _Fx();
with
    emit Z => 1;
with
    emit W => 0;
end
]],
    _ana = {
        reachs = 1,
        acc = 3,
        isForever = true,
    },
}

Test { [[
native _Fx;
deterministic _Fx with Z,W;
output int Z,W;
native do
    void Z() {};
end
par do
    _Fx();
with
    emit Z => 1;
with
    emit W => 0;
end
]],
    todo = true,
    _ana = {
        acc = 1,
        isForever = true,
    },
}

Test { [[
native _Fx;
output int&& Z,W;
deterministic _Fx with Z,W;
int a = 1;
int&& b;
native do
    void Z (int v) {};
end
par do
    _Fx(&&a);
with
    emit Z => b;
with
    emit W => &&a;
end
]],
    todo = true,
    _ana = {
        acc = 4,
        isForever = true,
    },
}

Test { [[
native _Fx;
@pure _Fx;
output int&& Z,W;
int a = 1;
int&& b;
native do
    void Z (int v) {};
end
par do
    _Fx(&&a);
with
    emit Z => b;
with
    emit W => &&a;
end
]],
    todo = true,
    _ana = {
        acc = 4,
        isForever = true,
    },
}

Test { [[
native _Fx;
deterministic Z with W;
output void Z,W;
par do
    emit Z;
with
    emit W;
end
]],
    todo = true,
    _ana = {
        reachs = 1,
        isForever = true,
    },
}

Test { [[
output Z  (var int)=>int;
escape call Z=>1;
]],
    parser = 'line 2 : after `call´ : expected expression',
    --parser = 'line 2 : after `call´ : expected expression',
    --parser = 'line 2 : after `Z´ : expected `;´',
    --parser = 'line 2 : after `Z´ : expected `(´',
}

Test { [[
output Z  (var int)=>int;
call Z=>1;
escape 1;
]],
    gcc = 'error: #error ceu_out_call_* is not defined',
}

Test { [[
output Z  (var int)=>int;
emit Z=>1;
escape 1;
]],
    env = 'line 2 : invalid `emit´',
    --run = 1,
}

Test { [[
native do
    ##define ceu_out_emit_F(a) Z(a)
    int Z (int v) {
        escape v+1;
    }
end
output Z  (var int)=>int;
call Z=>1;
escape 1;
]],
    gcc = 'error: #error ceu_out_call_* is not defined',
    --run = 1,
}

Test { [[
native do
    ##define ceu_out_call_F(a) Z((int*)a)
    int Z (int* v) {
        escape *v+1;
    }
end
output Z  (var int)=>int;
call Z=>1;
escape 1;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_call(a,b,c) Z((int*)c)
    int Z (int* v) {
        escape *v+1;
    }
end
output Z  (var int)=>int;
var int ret = call Z=>1;
escape ret;
]],
    run = 2,
}

Test { [[
native do
    ##define ceu_out_call_F(a) Z(a)
    int Z (int v) {
        escape v+1;
    }
end
output Z  (var int)=>int;
var int ret = call Z=>(1,2);
escape ret;
]],
    env = 'line 8 : arity mismatch',
    --env = 'line 8 : invalid attribution (void vs int)',
    --env = 'line 8 : invalid type',
}

Test { [[
output int E;
emit E=>(1,2);
escape 1;
]],
    env = 'line 2 : arity mismatch',
}

Test { [[
event (int) e;
emit e=>(1,2);
escape 1;
]],
    env = 'line 2 : arity mismatch',
}

Test { [[
event (int) e;
emit e;
escape 1;
]],
    env = 'line 2 : arity mismatch',
}

Test { [[
output int E;
emit E;
escape 1;
]],
    env = 'line 2 : arity mismatch',
}

Test { [[
output (int,int) E;
emit E=>1;
escape 1;
]],
    env = 'line 2 : arity mismatch',
}

Test { [[
event (int,int) e;
emit e=>(1);
escape 1;
]],
    env = 'line 2 : arity mismatch',
}

Test { [[
native do
    ##define ceu_out_call_F(a) Z(a)
    int Z (tceu__int__int* p) {
        escape p->_1 + p->_2;
    }
end
output Z  (var int, var int)=>int;
var int ret = call Z=>(1,2);
escape ret;
]],
    run = 3,
}

Test { [[
var int ret = (call Z=>2);
]],
    tops = 'line 1 : external "Z" is not declared',
}

Test { [[
native do
    ##define ceu_out_call(a,b,c) Z(a,b,c)
    int Z (tceu_app* app, tceu_nevt evt, int* p) {
        escape (evt == CEU_OUT_F) + *p;
    }
end
output Z  (var int)=>int;
var int ret = (call Z=>2);
escape ret;
]],
    run = 3,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) Z(a,b,d)
    int Z (tceu_app* app, tceu_nevt evt, void* p) {
        escape (evt==CEU_OUT_F && p==NULL);
    }
end
output void Z;
var int ret = (emit Z);
escape ret;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit_F() Z()
    int Z () {
        escape 1;
    }
end
output void Z;
var int ret = (emit Z);
escape ret;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) Z(a,b,d)
    int Z (tceu_app* app, tceu_nevt evt, int* p) {
        escape (evt == CEU_OUT_F) + *p;
    }
end
output int Z;
par/and do
    emit Z=>1;
with
    emit Z=>1;
end
escape 1;
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}

Test { [[
native do
    ##define ceu_out_call(a,b,c) Z(a,b,c)
    int Z (tceu_app* app, tceu_nevt evt, int* p) {
        escape (evt == CEU_OUT_F) + *p;
    }
end
output Z  (var int)=>int;
par/and do
    call Z=>1;
with
    call Z=>1;
end
escape 1;
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}

Test { [[
input Z,W  (var int a)=>int do
    escape a + 1;
end
]],
    parser = 'line 1 : after `Z´ : expected `(´',
    --adj = 'line 1 : same body for multiple declarations',
}

-- XXX
Test { [[
input Z  (var int a)=>int do
    escape a + 1;
end
input W  (var int a)=>int;
var int ret = call Z=>1;
escape ret;
]],
    tops = 'line 4 : input "W declared but not used',
}

Test { [[
input Z  (var int a)=>int do
    escape a + 1;
end
input W  (var int a)=>int;
var int ret = call Z=>1;
escape ret;
]],
    wrn = true,
    code = 'line 4 : missing function body',
    --run = 2,
}

Test { [[
input Z  (var int a)=>void do
    v = a;
end
var int v = 0;
call Z=>1;
escape v;
]],
    todo = 'globals',
    locs = 'line 2 : internal identifier "v" is not declared',
}

Test { [[
native/nohold _fprintf, _stderr;
var int v = 0;
input Z  (var int a)=>void do
    this.v = a;
    _fprintf(_stderr,"a=%d v=%d\n", a, v);
end
_fprintf(_stderr,"v=%d\n", v);
call Z=>1;
_fprintf(_stderr,"v=%d\n", v);
escape this.v;
]],
    todo = 'globals',
    run = 1,
}

Test { [[
var int v = 0;
input W  (var int a)=>int do
    escape a + 1;
end
input Z  (var int a)=>void do
    this.v = call W=>a;
end
call Z=>1;
escape this.v;
]],
    run = 2,
}

Test { [[
input (void,int) A;
escape 1;
]],
    wrn = true,
    env = 'line 1 : type cannot be `void´',
}
Test { [[
input (int,void) A;
escape 1;
]],
    wrn = true,
    env = 'line 1 : type cannot be `void´',
}
Test { [[
output (void,int) A;
escape 1;
]],
    wrn = true,
    env = 'line 1 : type cannot be `void´',
}
Test { [[
output (int,void) A;
escape 1;
]],
    wrn = true,
    env = 'line 1 : type cannot be `void´',
}

Test { [[
input A  (void)=>void do
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
input A  (var void a, var  int a)=>void do
    v = 1;
end
call A => ();
escape 1;
]],
    locs = 'line 1 : declaration of "a" hides previous declaration (tests.lua : line 1)',
}

Test { [[
input A  (var void a, var  int v)=>void do
    v = 1;
end
escape 1;
]],
    wrn = true,
    env = 'line 1 : type cannot be `void´',
}

Test { [[
input void OS_START;
var int v = 0;
input A  (void)=>void do
    v = 1;
end
call A;
escape v;
]],
    todo = 'global',
    run = 1,
}

Test { [[
input WRITE  (var int c)=>int do
    escape c + 1;
end
var byte b = 1;
var int ret = call WRITE => b;
escape ret;
]],
    run = 2,
}

Test { [[
native ___ceu_nothing;
input IA  (var int c)=>int do
    escape c + 1;
end
input IB  (var int c)=>void do
    ___ceu_nothing(&&c);
end
call IB => 0;
var int ret = call IA => 1;
escape ret;
]],
    run = 2,
}

--end -- OS (INPUT/OUTPUT)

-->>> OS_START

Test { [[
native _char,_assert;
native/pure _strcmp;
input (int,_char&& &&) OS_START;
var int argc;
var _char&& && argv;
(argc, argv) = await OS_START;
_assert(_strcmp(argv[1],"arg")==0);
escape argc;
]],
    --ana = 'line 3 : `loop´ iteration is not reachable',
    wrn = true,
    run = 2,
    args = 'arg',
}

Test { [[
input void OS_START;
event void e;
await OS_START;
emit e;
escape 10;
]],
    --ana = 'line 3 : `loop´ iteration is not reachable',
    wrn = true,
    run = 10,
}

Test { [[
input void OS_START;
event void e;
loop do
    await OS_START;
    loop i in [0 |> 10[ do
        emit e;
    end
    do break; end
end
escape 10;
]],
    --ana = 'line 3 : `loop´ iteration is not reachable',
    wrn = true,
    run = 10,
}

--<<< OS_START

Test { [[
input (int) X;
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
input (int);
]],
    parser = 'line 1 : after `)´ : expected external identifier',
}
Test { [[
input (xxx);
]],
    parser = 'line 1 : after `(´ : expected type',
}
Test { [[
input ();
]],
    parser = 'line 1 : after `(´ : expected type',
}
Test { [[
input (u8);
]],
    parser = 'line 1 : after `)´ : expected external identifier',
}

Test { [[
input (xxx tilex) X;;
]],
    parser = 'line 1 : after `(´ : expected type',
}

Test { [[

input (int tilex, int tiley, bool vertical, int lock, int door, usize&& position) DOOR_SPAWN;

    var int tilex;
    var int tiley;
    var bool vertical;
    var int lock;
    var int door;
    var usize&& position;
    every (tilex,tiley,vertical,lock,door,position) in DOOR_SPAWN do
    end
]],
    parser = 'line 2 : after `int´ : expected type modifier or `,´ or `)´',
}

    -- POINTERS & ARRAYS

-- int_int
Test { [[var int&&p; escape p/10;]],  env='invalid operands to binary "/"'}
Test { [[var int&&p; escape p|10;]],  env='invalid operands to binary "|"'}
Test { [[var int&&p; escape p>>10;]], env='invalid operands to binary ">>"'}
Test { [[var int&&p; escape p^10;]],  env='invalid operands to binary "^"'}
Test { [[var int&&p; escape ~p;]],    env='invalid operand to unary "~"'}

-- same
Test { [[var int&&p; var int a; escape p==a;]],
        env='invalid operands to binary "=="'}
Test { [[var int&&p; var int a; escape p!=a;]],
        env='invalid operands to binary "!="'}
Test { [[var int&&p; var int a; escape p>a;]],
        env='invalid operands to binary ">"'}

-- any
Test { [[var int&&p=null; escape p or 10;]], run=1 }
Test { [[var int&&p=null; escape p and 0;]],  run=0 }
Test { [[var int&&p=null; escape not p;]], run=1 }

-- arith
Test { [[var int&&p; escape p+p;]],     env='invalid operands to binary'}--TODO: "+"'}
Test { [[var int&&p; escape p+10;]],    env='invalid operands to binary'}
Test { [[var int&&p; escape p+10 and 0;]], env='invalid operands to binary' }

-- ptr
Test { [[var int a; escape *a;]], env='invalid operand to unary "*"' }
Test { [[var int a; var int&&pa; (pa+10)=&&a; escape a;]],
    parser = 'TODO: exp-lval',
    --env='invalid operands to binary'
}
Test { [[var int a; var int&&pa; a=1; pa=&&a; *pa=3; escape a;]], run=3 }

Test { [[
native _V;
*(0x100 as u32&&) = _V;
escape 1;
]],
    gcc = 'error: ‘V’ undeclared (first use in this function)',
}

Test { [[var int  a;  var int&& pa=a; escape a;]], env='types mismatch' }
Test { [[var int&& pa; var int a=pa;  escape a;]], env='types mismatch' }
Test { [[
var int a;
var int&& pa = do/_
    escape a;
end;
escape a;
]],
    env='types mismatch'
}
Test { [[
var int&& pa;
var int a = do/_
    escape pa;
end;
escape a;
]],
    env='types mismatch'
}

Test { [[
var int&& a;
a = null;
if a then
    escape 1;
else
    escape -1;
end;
]],
    run = -1,
}

Test { [[
native _char;
var int i;
//var int&& pi;
var _char c=10;
//var _char&& pc;
i = c;
c = i;
i = (c as int);
c = (i as _char);
escape c;
]],
    --env = 'line 6 : invalid attribution',
    run = 10,
}

Test { [[
native _char;
var int i;
//var int&& pi;
var _char c=0;
//var _char&& pc;
i = (c as int);
c = (i as _char);
escape 10;
]],
    run = 10
}

Test { [[
var int&& ptr1=null;
var void&& ptr2=null;
if true then
    ptr2 = ptr1;
else
    ptr2 = ptr2;
end;
escape 1;
]],
    --gcc = 'may be used uninitialized in this function',
    --fin = 'line 6 : pointer access across `await´',
    run = 1,
}

Test { [[
var int&& ptr1 = null;
var void&& ptr2 = null;
if true then
    ptr2 = ptr1 as void&&;
else
    ptr2 = ptr2;
end;
escape 1;
]],
    --fin = 'line 6 : pointer access across `await´',
    run = 1,
}

Test { [[
var int&& ptr1;
var void&& ptr2=null;
ptr1 = ptr2 as int&&;
ptr2 = ptr1 as void&&;
escape 1;
]],
    run = 1,
}

Test { [[
var void&& ptr1;
var int&& ptr2=null;
ptr1 = ptr2;
ptr2 = ptr1;
escape 1;
]],
    env = 'line 4 : types mismatch (`int&&´ <= `void&&´)',
    run = 1,
}

Test { [[
native _char;
var _char&& ptr1;
var int&& ptr2=0xFF as void&&;
ptr1 = ptr2;
ptr2 = ptr1;
escape ptr2 as int;
]],
    env = 'line 3 : types mismatch (`int&&´ <= `void&&´)',
    --env = 'line 4 : invalid attribution',
    --run = 255,
    gcc = 'error: assignment from incompatible pointer type'
}
Test { [[
native _char;
var _char&& ptr1;
var int&& ptr2=0xFF as void&&;
ptr1 = (ptr2 as _char&&);
ptr2 =  ptr1 as int&&;
escape (ptr2 as int);
]],
    env = 'line 3 : types mismatch (`int&&´ <= `void&&´)',
    --env = 'line 4 : invalid attribution',
    --run = 255,
    gcc = 'error: cast from pointer to integer of different size',
}
Test { [[
native _char;
var _char&& ptr1;
var int&& ptr2=null;
ptr1 = (ptr2 as _char&&);
ptr2 = ptr1 as int&&;
escape 1;
]],
    run = 1,
}
Test { [[
native _char;
var int&& ptr1;
var _char&& ptr2=null;
ptr1 =  ptr2 as int&&;
ptr2 = ( ptr1 as _char&&);
escape 1;
]],
    run = 1,
}

Test { [[
native _FILE;
var int&& ptr1;
var _FILE&& ptr2=null;
ptr1 = ptr2;
ptr2 = ptr1;
escape 1;
]],
    --env = 'line 4 : invalid attribution (int&& vs _FILE&&)',
    gcc = 'error: assignment from incompatible pointer type',
    --run = 1,
    --env = 'line 4 : invalid attribution',
}

Test { [[
native _FILE;
var int&& ptr1;
var _FILE&& ptr2=null;
ptr1 = ptr2 as int&&;
ptr2 = ptr1 as _FILE&&;
escape 1;
]],
    run = 1,
    --env = 'line 4 : invalid attribution',
}

Test { [[
var int a = 1;
var int&& b = &&a;
*b = 2;
escape a;
]],
    run = 2,
}

Test { [[
var int a=0;
var int&& pa=null;
par/or do
    a = 1;
with
    pa = &&a;
end;
escape a;
]],
    _ana = {
        abrt = 1,
    },
    run = 1,
}

Test { [[
var int b = 1;
var int&& a = &&b;
par/or do
    b = 1;
with
    *a = 0;
end
escape b;
]],
    _ana = {
        abrt = 1,
        acc = 1,
    },
    run = 1,
}

Test { [[
var int b = 1;
var int c = 2;
var int&& a = &&c;
deterministic b with a, c;
par/and do
    b = 1;
with
    *a = 3;
end
escape *a+b+c;
]],
    run = 7,
}

Test { [[
native/nohold _f;
native do
    void f (int* v) {
        *v = 1;
    }
end
var int a=1, b=1;
par/and do
    _f(&&b);
with
    _f(&&a);
end
escape a + b;
]],
    run = 2,
    _ana = {
        acc = 1,
    },
}

Test { [[
native/nohold _f;
native do
    void f (int* v) {
        *v = 1;
    }
end
var int a=1, b=1;
var int&& pb = &&b;
par/and do
    a = 1;              // 10
with
    _f(&&b);             // 12
with
    _f(&&a);             // 14
with
    _f(pb);             // 16
end
escape a + b;
]],
    run = 2,
    _ana = {
        acc = 9,
    },
}

Test { [[
native/nohold _f;
native do
    void f (int* v) {
        *v = 1;
    }
end
var int a=1, b=0;
var int&& pb = &&b;
par/or do
    a = 1;
with
    _f(&&b);
with
    _f(&&a);
with
    _f(pb);
end
escape a + b;
]],
    run = 1,
    _ana = {
        abrt = 6,
        acc = 9,
    },
}

Test { [[
@pure _f;
native do
    void f (int* v) {
        *v = 1;
    }
end
var int a=1, b=1;
var int&& pb = &&b;
par/and do
    a = 1;
with
    _f(&&b);
with
    _f(&&a);
with
    _f(pb);
end
escape a + b;
]],
    todo = true,
    run = 2,
    _ana = {
        acc = 2,
    },
}

Test { [[
@pure _f;
native do
    void f (int* v) {
        *v = 1;
    }
end
var int a=1, b=1;
var int&& pb = &&b;
par/or do
    a = 1;
with
    _f(&&b);
with
    _f(&&a);
with
    _f(pb);
end
escape a + b;
]],
    todo = true,
    run = 1,
    _ana = {
        acc = 2,
    },
}

Test { [[
var int b = 10;
var int&& a = (&&b as int&&);
var int&& c = &&b;
escape *a + *c;
]],
    run = 20;
}

Test { [[
native _f;
native do
    int* f () {
        int a = 10;
        escape &a;
    }
end
var& int? p = _f();
escape p;
]],
    env = 'line 8 : invalid attribution : missing `!´ (in the left) or `&´ (in the right)',
}

Test { [[
native _f;
native do
    int* f () {
        int a = 10;
        escape &a;
    }
end
var& int? p = &_f();
escape p;
]],
    env = 'line 9 : types mismatch (`int´ <= `int&?´)',
}

Test { [[
native _f;
native do
    int* f () {
        int a = 10;
        escape &a;
    }
end
var& int? p = &_f();
escape p!;
]],
    fin = 'line 8 : attribution requires `finalize´',
}

Test { [[
native _f;
native do
    int a;
    int* f () {
        a = 10;
        escape &a;
    }
end
var& int? p;
do
    p = &_f();
finalize
with
    nothing;
end
escape p!;
]],
    run = 10,
}
Test { [[
native _f;
native do
    int a;
    int* f () {
        a = 10;
        escape &a;
    }
end
var& int? p;
do
    p = &_f();
finalize
with
    nothing;
end
escape p!;
]],
    run = 10,
}
Test { [[
native/pure _f;    // its actually impure
native do
    int a;
    int* f () {
        a = 10;
        escape &a;
    }
end
var int&& p;
    p = _f();
escape *p;
]],
    run = 10,
}
Test { [[
native _f;
native do
    int A = 10;
    int* f () {
        escape &A;
    }
end
var int a=0;
do
    var& int? p;
do
        p = &_f();
    finalize
    with
        a = p!;
end
end
escape a;
]],
    run = 10,
}

Test { [[
native _f;
native do
    int A = 10;
    int* f () {
        escape &A;
    }
end
var int a = 10;
do
    var& int? p;
    //do
do
            p = &_f();
        finalize
        with
            a = a + p!;
        end
    //end
    a = 0;
end
escape a;
]],
    run = 10,
}

Test { [[
native _f, _p;
native do
    ##define f(p)
end
par/or do
do _f(_p);
        finalize with
            _f(null);
        end;
with
    await FOREVER;
end
escape 1;
]],
    fin = 'line 5 : invalid `finalize´',
    --run = 1,
}

Test { [[
native _f, _p;
native do
    ##define f(p)
end
par/or do
    _f(_p);
with
    await FOREVER;
end
escape 1;
]],
    run = 1,
}

Test { [[
native _char;
var _char&& p=null;
*(p:a) = (1 as _char);
escape 1;
]],
    --env = 'line 3 : invalid operand to unary "*"',
    gcc = 'error: request for member',
}

Test { [[
input void OS_START;
var int h = 10;
var& int p = &h;
do
    var int x = 0;
    await OS_START;
    var int z = 0;
    if x and z then end;
end
escape p;
]],
    run = 10;
}

-->>> NATIVE/POINTERS/VECTORS

Test { [[input int[1] E; escape 0;]],
    --run = 0,
    --env = 'invalid event type',
    parser = 'line 1 : after `int´ : expected type modifier or external identifier',
}
Test { [[vector[0] int v; escape 0;]],
    run = 0,
    --env='invalid dimension'
}
Test { [[vector[2] int v; escape v;]],
    env = 'types mismatch'
}
Test { [[native _u8; var _u8[2] v; escape &&v;]],
    env = 'line 1 : types mismatch (`int´ <= `_u8[]&&´)',
    --env = 'invalid operand to unary "&&"',
}
Test { [[vector[2] u8 v; escape &&v;]],
    env = 'line 1 : types mismatch (`int´ <= `u8[]&&´)',
    --env = 'invalid operand to unary "&&"',
}

Test { [[
N;
]],
    --adj = 'line 1 : invalid expression',
    --parser = 'line 1 : after `<BOF>´ : expected statement',
    parser = 'line 1 : after `begin of file´ : expected statement',
    --parser = 'after `N´ : expected `(´',
}

Test { [[
void[10] a;
]],
    parser = 'line 1 : after `begin of file´ : expected statement',
}

Test { [[
vector[10] void a;
]],
    env = 'line 1 : cannot instantiate type "void"',
}

Test { [[
native _int;
var _int[2] v = [];
v[0] = 5;
escape v[0];
]],
    run = 5
}

Test { [[
native _int;
var _int[2] v = [];
v[0] = 1;
v[1] = 1;
escape v[0] + v[1];
]],
    run = 2,
}

Test { [[
native _int;
var _int[2] v = [];
var int i;
v[0] = 0;
v[1] = 5;
v[0] = 0;
i = 0;
escape v[i+1];
]],
    run = 5
}

Test { [[
var void a;
vector[1] void b;
]],
    env = 'line 1 : cannot instantiate type "void"',
}

Test { [[
var int a;
vector[1] void b;
]],
    env = 'line 2 : cannot instantiate type "void"',
}

Test { [[
pre native do
    typedef struct {
        int v[10];
        int c;
    } Tx;
end
native _Tx ;

var _Tx[10] vec = [];
var int i = 110;

vec[3].v[5] = 10;
vec[9].c = 100;
escape i + vec[9].c + vec[3].v[5];
]],
    run = 220,
}

Test { [[
var int i = do/_
    vector[5] byte abcd;
    escape 1;
end;
escape i;
]],
    run = 1,
}

Test { [[
native _int;
var _int[1] v;
escape 1;
]],
    --run = 1,
    --cval = 'line 1 : invalid dimension',
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:1)',
}
Test { [[
native _u8, _V;
var _u8[10] v = [_V];
escape v[0];
]],
    env = 'invalid attribution : external vectors accept only empty initialization `[]´',
}

Test { [[
native _u8;
var _u8[10] vvv = [];
vvv[9] = 1;
escape vvv[9];
]],
    run = 1,
}

Test { [[
native _u8;
var _u8[10] v;
escape v[0];
]],
    ref = 'line 2 : invalid access to uninitialized variable "v" (declared at tests.lua:1)',
}

Test { [[
native _u8;
data Test with
  var _u8[10] v;
end
var Test t = Test();
escape t.v[0];
]],
    env = 'line 4 : arity mismatch',
}

Test { [[
native _u8;
data Test with
  var _u8[10] v;
end
var Test t = Test([]);
t.v[9] = 10;
escape t.v[9];
]],
    run = 10,
}

Test { [[
native _u8;
data Test with
    var int a;
    var _u8[10] v;
    var int b;
end
var Test t = Test(1, [], 1);
t.v[0] = 10;
escape t.v[0];
]],
    run = 10,
}

Test { [[vector[2] int v; await v;     escape 0;]],
        env='event "v" is not declared' }
Test { [[vector[2] int v; emit v;    escape 0;]],
        env = 'line 1 : identifier "v" is not an event (tests.lua : line 1)',
}
Test { [[vector[0] int[2] v; await v;  escape 0;]],
        --env='line 1 : event "?" is not declared'
        parser = 'line 1 : after `int´ : expected type modifier or internal identifier',
}
Test { [[vector[0] int[2] v; emit v; escape 0;]],
        --env='event "?" is not declared'
        parser = 'line 1 : after `int´ : expected type modifier or internal identifier',
}
Test { [[native _int; var _int[2] v; v=v; escape 0;]], env='types mismatch' }
Test { [[vector[1] int v; escape v;]], env='cannot index a non array' }
Test { [[native _int; var _int[2] v; escape v[v];]], env='invalid array index' }

Test { [[
native _int;
var _int[2] v ;
escape v == &&v[0] ;
]],
    env = 'line 2 : invalid operands to binary "=="',
    --run = 1,
}

Test { [[
native _int;
native/nohold _f;
native do
    void f (int* p) {
        *p = 1;
    }
end
var _int[2] a = [];
var int b=0;
par/and do
    b = 2;
with
    _f(&&a);
end
escape a[0] + b;
]],
    run = 3,
}

Test { [[
native/nohold _f;
native do
    void f (int* p) {
        *p = 1;
    }
end
native _int;
var _int[2] a = [];
a[0] = 0;
a[1] = 0;
var int b=0;
par/and do
    b = 2;
with
    _f(a);
end
escape a[0] + b;
]],
    _ana = {
        abrt = 1,
    },
    --env = 'line 14 : wrong argument #1 : cannot pass plain vectors to native calls',
    --code = 'line 14 : invalid value : vectors are not copyable',
    run = 3,
}

Test { [[
native/nohold _f;
native do
    void f (int* p) {
        *p = 1;
    }
end
native _int;
var _int[2] a = [];
a[0] = 0;
a[1] = 0;
var int b=0;
par/or do
    b = 2;
with
    _f(&&a);
end
escape a[0] + b;
]],
    _ana = {
        abrt = 1,
    },
    run = 2,
}

Test { [[
vector[255] u8 vec;
event void  e;
escape 1;
]],
    --mem = 'too many events',    -- TODO
    run = 1,
}

local evts = ''
for i=1, 256 do
    evts = evts .. 'event int e'..i..';\n'
end
Test { [[
]]..evts..[[
escape 1;
]],
    env = 'line 1 : too many events',
}

Test { [[
var int a = 1;
escape a;
]],
    run = 1,
}

Test { [[
input (byte&&, u32) HTTP_GET;
var byte&& p2Buff;
var u32 len;
(p2Buff, len) = await HTTP_GET;
vector[0] byte c = p2Buff; // doesn't work
escape 1;
]],
    env = 'line 5 : cannot index pointers to internal types',
}

Test { [[
native _char;
input (byte&&, u32) HTTP_GET;
par/or do
    var _char&& p2Buff;
    var u32 len;
    (p2Buff, len) = await HTTP_GET;
    vector[0] byte c = p2Buff; // doesn't work
    if len and p2Buff and c then end;
with
end
escape 1;
]],
    run = 1,
}

Test { [[
native/pure _f;
native do
    int f (int* v) {
        escape v[0];
    }
end
native _int;
var _int[2] v = [];
v[0] = 10;
escape _f(v);
]],
    run = 10,
}

Test { [[
native _ceu_uv_read_start, _assert;
input void UV_READ;
native/plain _char, _uv_buf_t, _uv_stream_t;
native/nohold _uv_buf_init, _uv_read_stop;
var _char[3] buf_ = [];
var _uv_buf_t buf = _uv_buf_init(buf_, sizeof(buf_)-1);
var _uv_stream_t client = _uv_stream_t();
var int ret;
do ret = _ceu_uv_read_start(&&client as _uv_stream_t&&, &&buf);
                    finalize with
                        _uv_read_stop(&&client as _uv_stream_t&&);
                    end;
_assert(ret == 0);
escape 0;
]],
    wrn = true,
    gcc = 'implicit declaration of function ‘uv_buf_init’',
}

Test { [[
native _char;

data Tx with
    var _char[255] str;
    var int x;
end
var Tx t = Tx([], 1);
t.str[0] = '\0';
escape t.x;
]],
    run = 1,
}

Test { [[
native _char;
native/pure _strlen;
data Tx with
    var _char[255] xxxx;
end
var Tx t = Tx("oioioi");
escape _strlen(t.xxxx);
]],
    run = 6,
}

Test { [[
native/pure _strlen;

native _char;
var _char[255] str;
str = "oioioi";

escape _strlen(&&str);
]],
    gcc = '4:34: error: assignment to expression with array type',
}

--<<< NATIVE/POINTERS/VECTORS

-->>> VECTORS / STRINGS

Test { [[
var u8 v;
escape $$v;
]],
    env = 'line 2 : invalid operand to unary "$$" : vector expected',
}
Test { [[
var u8 v;
escape $v;
]],
    env = 'line 2 : invalid operand to unary "$" : vector expected',
}

Test { [[
vector[10] u8 vec;
escape $$vec + $vec;
]],
    run = 10,
}

Test { [[
vector[] u8 vec;
escape $$vec + $vec + 1;
]],
    run = 1,
}

Test { [[
vector[] int c;
escape [];
]],
    parser = 'line 2 : after `escape´ : expected expression',
    --env = 'line 2 : invalid attribution : destination is not a vector',
    --env = 'line 2 : types mismatch (`int´ <= `any[]´)',
}

Test { [[
vector[] int c;
escape [1]..[]..c;
]],
    parser = 'line 2 : after `escape´ : expected expression',
    --env = 'line 2 : invalid attribution : destination is not a vector',
    --env = 'line 2 : types mismatch (`int´ <= `int[]´)',
}

Test { [[
vector[10] u8 vec = [ [1,2,3] ];
escape 1;
]],
    parser = 'line 1 : after `[´ : expected `]´',
    --parser = 'line 1 : after `[´ : expected `]´',
    --env = 'line 1 : wrong argument #1 : arity mismatch',
    --env = 'line 1 : types mismatch (`u8[]´ <= `int[][]´)',
    --env = 'line 1 : wrong argument #1 : types mismatch (`u8´ <= `int[]..´)',
}
Test { [[
vector[10] u8 vec = (1,2,3);
escape 1;
]],
    parser = 'line 1 : after `1´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `)´',
}
Test { [[
vector[10] u8 vec = (1);
escape 1;
]],
    env = 'line 1 : types mismatch (`u8[]´ <= `int´)',
}
Test { [[
native _int;
var _int[1] vec = [];
escape 1;
]],
    --env = 'line 1 : invalid attribution : destination is not a vector',
    run = 1,
}

Test { [[
var int x;
vector[10] u8 vec = [ &&x ];
escape 1;
]],
    env = 'line 2 : wrong argument #1 : types mismatch (`u8´ <= `int&&´)',
}

Test { [[
vector[] int v = [] ..;
escape 1;
]],
    --parser = 'line 1 : after `..´ : expected item',
    --parser = 'line 1 : after `..´ : invalid constructor syntax',
    parser = 'line 1 : after `..´ : expected expression or `[´',
}

Test { [[
vector[] int&& v1;
vector[] int  v2 = []..v1;
escape 1;
]],
    env = 'line 2 : wrong argument #2 : types mismatch (`int´ <= `int&&´)',
}

Test { [[
vector[10] u8 vec = [1,2,3];
escape $$vec + $vec + vec[0] + vec[1] + vec[2];
]],
    run = 19,
}

Test { [[
vector[10] u8 vec = [1,2,3];
vec[0] = 4;
vec[1] = 5;
vec[2] = 6;
escape $$vec + $vec + vec[0] + vec[1] + vec[2];
]],
    run = 28,
}

Test { [[
vector[10] int vec = [1,2,3];
vec[0] = 4;
vec[1] = 5;
vec[2] = 6;
escape $$vec + $vec + vec[0] + vec[1] + vec[2];
]],
    run = 28,
}

Test { [[
vector[10] u8 vec;
vec[0] = 1;
escape 1;
]],
    run = '2] runtime error: access out of bounds',
}

Test { [[
vector[10] u8 vec;
escape vec[0];
]],
    run = '2] runtime error: access out of bounds',
}

Test { [[
vector[] u8 vec = [1,2,3];
escape $$vec + $vec + vec[0] + vec[1] + vec[2];
]],
    run = 6,
}

Test { [[
vector[10] u8 vec = [1,2,3];
$$vec = 0;
escape vec[0];
]],
    parser = 'TODO: exp-lval',
    --env = 'line 2 : invalid attribution',
}
Test { [[
vector[10] u8 vec = [1,2,3];
$vec = 0;
escape vec[0];
]],
    run = '3] runtime error: access out of bounds',
}

Test { [[
vector[2] int vec;
$vec = 1;
escape 1;
]],
    run = '2] runtime error: invalid attribution : out of bounds',
}

Test { [[
vector[] byte bs;
$bs := 1;
escape $bs;
]],
    run = 1,
}

Test { [[
vector[10] byte bs;
$bs := 10;
escape $bs;
]],
    run = 10,
}

Test { [[
vector[10] byte bs;
$bs := 11;
escape $bs;
]],
    run = '2] runtime error: invalid attribution : out of bounds',
}

Test { [[
vector[10] u8 v1 = [1,2,3];
vector[20] u8 v2 = v1;
escape v2[0] + v2[1] + v2[2];
]],
    env = 'line 2 : types mismatch (`u8[]´ <= `u8[]´)',
}

Test { [[
vector[] byte v1, v2, v3;
v1 = v2;
v1 = v2..v3;
escape 1;
]],
    parser = 'line 3 : after `v2´ : expected `(´ or `[´ or `:´ or `?´ or `!´ or binary operator or `;´',
}

Test { [[
vector[10] u8 v1 = [1,2,3];
vector[20] u8 v2 = []..v1;
escape v2[0] + v2[1] + v2[2];
]],
    run = 6,
}
Test { [[
vector[20] u8 v1 = [1,2,3];
vector[10] u8 v2 = []..v1;
escape v2[0] + v2[1] + v2[2];
]],
    run = 6,
}
Test { [[
vector[] u8 v1   = [1,2,3];
vector[10] u8 v2 = []..v1;
escape v2[0] + v2[1] + v2[2];
]],
    run = 6,
}
Test { [[
vector[10] u8 v1 = [1,2,3];
vector[] u8   v2 = []..v1;
escape v2[0] + v2[1] + v2[2];
]],
    run = 6,
}
Test { [[
vector[3] u8 v1 = [1,2,3];
vector[2] u8 v2 = []..v1;
escape v2[0] + v2[1] + v2[2];
]],
    run = '2] runtime error: access out of bounds',
}

Test { [[
vector[10] u8 vec = [1,2,3];
vector&[] u8  ref = &vec;
escape $$ref + $ref + ref[0] + ref[1] + ref[2];
]],
    run = 19,
}

Test { [[
vector[10] u8  vec = [1,2,3];
vector&[11] u8 ref = &vec;
escape $$ref + $ref + ref[0] + ref[1] + ref[2];
]],
    run = 1,
    env = 'line 2 : types mismatch (`u8[]&´ <= `u8[]&´) : dimension mismatch',
}

Test { [[
vector[10] u8 vec = [1,2,3];
vector&[9] u8 ref = &vec;
escape $$ref + $ref + ref[0] + ref[1] + ref[2];
]],
    env = 'line 2 : types mismatch (`u8[]&´ <= `u8[]&´) : dimension mismatch',
}

Test { [[
vector[2] int v ;
escape v == &&v[0] ;
]],
    env = 'line 2 : invalid operand to unary "&&" : vector elements are not addressable',
}

Test { [[
native/nohold _f;
native do
    void f (int* v) {
        v[0]++;
        v[1]++;
    }
end
vector[2] int a = [1,2];
native _int;
_f(&&a as _int&&);
escape a[0] + a[1];
]],
    run = 5,
}

Test { [[
native/nohold _f;
native do
    void f (int* v) {
        v[0]++;
        v[1]++;
    }
end
vector[2] int a  = [1,2];
vector&[2] int b = &a;
native _char;
_f(&&b as _char&&);
escape b[0] + b[1];
]],
    env = 'line 10 : invalid type cast',
}

Test { [[
native/nohold _f;
native do
    void f (int* v) {
        v[0]++;
        v[1]++;
    }
end
vector[2] int a  = [1,2];
vector&[2] int b = &a;
native _int;
_f(&&b as _int&&);
escape b[0] + b[1];
]],
    run = 5,
}

Test { [[
vector[] byte bs = [ 1, 2, 3 ];
var int idx = 1;
var& int i = &idx;
escape bs[i];
]],
    run = 2,
}

Test { [[
vector[5] u8 foo = [1, 2, 3, 4, 5];
var int tot = 0;
loop i in [0 |> $foo[ do
    tot = tot + foo[i];
end
escape tot;
]],
    tight = 'line 3 : tight loop',
}
Test { [[
vector[5] u8 foo = [1, 2, 3, 4, 5];
var int tot = 0;
loop i in [0 |> $foo[ do
    tot = tot + foo[i];
end
escape tot;
]],
    loop = true,
    wrn = true,
    run = 15,
}

Test { [[
vector[5] u8 foo = [1, 2, 3, 4, 5];
var int tot = 0;
loop i in [0 |> $$foo[ do
    tot = tot + foo[i];
end
escape tot;
]],
    run = 15,
}

Test { [[
vector[] u8 foo = [1, 2, 3, 4, 5];
var int tot = 0;
loop i in [0 |> $$foo[ do
    tot = tot + foo[i];
end
escape tot+1;
]],
    run = 1,
}

Test { [[
escape 1..2;
]],
    --parser = 'line 1 : after `..´ : invalid constructor syntax',
    parser = 'line 1 : after `1´ : expected `(´ or `[´ or `:´ or `?´ or `!´ or binary operator or `;´',
}
Test { [[
escape 1 .. 2;
]],
    --parser = 'line 1 : after `..´ : invalid constructor syntax',
    --parser = 'line 1 : after `1´ : expected `;´',
    parser = 'line 1 : after `1´ : expected `(´ or `[´ or `:´ or `?´ or `!´ or binary operator or `;´',
}
Test { [[
vector[] int x = [1]..2;
escape 1;
]],
    env = 'line 1 : wrong argument #2 : source is not a vector',
}

Test { [[
escape [1]..[2];
]],
    parser = 'line 1 : after `escape´ : expected expression',
    --env = 'line 1 : invalid attribution : destination is not a vector',
}

Test { [[
escape [1]..[&&this];
]],
    --env = 'line 1 : invalid attribution : destination is not a vector',
    parser = 'line 1 : after `escape´ : expected expression',
}

Test { [[
vector[] int v1;
vector[] int v2;
v1 = [1] .. v2;
v1 = [] .. v2 .. [1];
escape v1[0];
]],
    run = 1;
}

Test { [[
vector[] int v1 = [1]..[2]..[3];
escape v1[0]+v1[1]+v1[2];
]],
    run = 6;
}

Test { [[
vector[] int v1 = [1,2,3];
vector[] int v2 = [7,8,9];
v1 = [] .. v1 .. [4,5,6] .. v2;
var int ret = 0;
loop i in [0 |> 9[ do
    ret = ret + v1[i];
end
escape ret;
]],
    run = 45;
}

Test { [[
vector[] int v = [1,2,3];
v = [] .. v .. v;
escape $v + v[5];
]],
    run = 9,
}

Test { [[
vector[] int v = [1,2,3];
v = [1] .. v;
escape $v + v[1];
]],
    run = 3,
}

Test { [[
vector[] int v;
$v = 0;
escape $v + 1;
]],
    run = 1,
}
Test { [[
data SDL_Rect with
    var int x;
end
vector[] SDL_Rect cell_rects;
escape 1;
]],
    run = 1,
}

Test { [[
data SDL_Rect with
    var int x;
end
var SDL_Rect r1 = SDL_Rect(10);
vector[] SDL_Rect cell_rects = [r1];
escape cell_rects[0].x;
]],
    run = 10,
}

Test { [[
native do
    byte* f (void) {
        escape "ola";
    }
    typedef struct {
        byte* (*f) (void);
    } tp;
    tp Tx = { f };
end
vector[] byte str = [] .. "oi";
escape str[1]=='i';
]],
    run = 1,
}

Test { [[
native do
    byte* f (void) {
        escape "ola";
    }
    typedef struct {
        byte* (*f) (void);
    } tp;
    tp Tx = { f };
end
native _char, _Tx;
vector[] byte str = [] .. (_Tx.f() as _char&&);
escape str[2]=='a';
]],
    run = 1,
}

Test { [[
native do
    byte* f (void) {
        escape "ola";
    }
    typedef struct {
        byte* (*f) (void);
    } tp;
    tp Tx = { f };
end
native _char, _Tx;
vector[] byte str = [] .. (_Tx.f() as _char&&) .. "oi";
escape str[4]=='i';
]],
    run = 1,
}

Test { [[
native do
    byte* f (void) {
        escape "ola";
    }
end
vector[] byte  str;
vector&[] byte ref = &str;
native _char;
ref = [] .. ({f}() as _char&&) .. "oi";
native/pure _strlen;
escape _strlen(&&str as _char&&);
]],
    run = 5,
}

-- TODO: dropped support for returning alias, is this a problem?

Test { [[
vector[] byte str = [0,1,2];

code/instantaneous Fx (void) => byte[]& do
    escape &this.str;
end

vector&[] byte ref = &f();

escape ref[1];
]],
    parser = 'line 3 : after `byte´ : expected type modifier or `;´ or `do´',
    --run = 1,
}

Test { [[
vector[] byte str = [0,1,2];

code/instantaneous Fx (void) => byte[]& do
    escape &this.str;
end

vector&[] byte ref = &f();
ref = [3, 4, 5];

escape str[1];
]],
    parser = 'line 3 : after `byte´ : expected type modifier or `;´ or `do´',
    --run = 4,
}

Test { [[
vector[] byte str = [0,1,2];

code/instantaneous Fx (void) => byte[]& do
    escape &this.str;
end

vector&[] byte ref = &f();
ref = [] .. "ola";

escape str[1] == 'l';
]],
    parser = 'line 3 : after `byte´ : expected type modifier or `;´ or `do´',
    --run = 1,
}

Test { [[
vector[] byte str = [0,1,2];

native do
    byte* g () {
        escape "ola";
    }
end

code/instantaneous Fx (void) => byte[]& do
    escape &this.str;
end

vector&[] byte ref = &f();
native _char;
ref = [] .. ({g}() as _char&&) .. "ola";

escape str[3] == 'o';
]],
    --run = 1,
    parser = 'line 9 : after `byte´ : expected type modifier or `;´ or `do´',
}

Test { [[
vector[] byte str;

code/instantaneous Fa (void)=>byte[]& do
    escape &this.str;
end

code/instantaneous Fb (void)=>void do
    vector&[] byte ref = &f1();
    ref = [] .. "ola" .. "mundo";
end

f2();

escape str[4] == 'u';
]],
    parser = 'line 3 : after `byte´ : expected type modifier or `;´ or `do´',
    --run = 1,
}

Test { [[
native do
    ##define ID(x) x
end
native/pure _ID, _strlen;
native _char;
vector[] byte str = [] .. "abc"
                    .. (_ID("def") as _char&&);
var byte&& str2 = _ID(&&str as _char&&);
escape _strlen(&&str as _char&&) + _strlen(str2);
]],
    run = 12,
}

Test { [[
vector[] byte str;
vector[] byte str;
escape 1;
]],
    wrn = true,
    run = 1,
}


Test { [[
native/pure _strcmp;
vector[] byte str1;
vector[] byte str2 = [].."";
native _char;
escape _strcmp(&&str1 as _char&&,"")==0 and _strcmp(&&str2 as _char&&,"")==0;
]],
    run = 1,
}

Test { [[
native _strlen;
code/instantaneous Strlen (var byte&& str)=>int do
    escape _strlen(str);
end

vector[] byte str = [].."Ola Mundo!";
escape Strlen(&&str);
]],
    env = 'line 6 : wrong argument #1 : types mismatch (`byte&&´ <= `byte[]&&´)',
}

Test { [[
native _char, _strlen;
code/instantaneous Strlen (var byte&& str)=>int do
    escape _strlen(str);
end

vector[] byte str = [].."Ola Mundo!";
escape Strlen(&&str as _char&&);
]],
    run = 10,
}

Test { [[
vector[3] u8 bytes;

bytes = [] .. bytes .. [5];

escape bytes[0];
]],
    run = 5,
}

Test { [[
native/nohold _ceu_vector_copy_buffer;
vector[] byte v = [1,2,0,4,5];
var byte c = 3;
_ceu_vector_copy_buffer(&&v, 2, &&c, 1, 1);
escape v[2] + $v;
]],
    run = 8,
}

Test { [[
native/nohold _ceu_vector_copy_buffer;
vector[5] byte v = [1,2,0,4,5];
var byte c = 3;
var int ok = _ceu_vector_copy_buffer(&&v, 2, &&c, 1, 1);
escape v[2] + $v + ok;
]],
    run = 9,
}

Test { [[
native/nohold _ceu_vector_copy_buffer;
vector[5] byte v = [1,2,1,4,5];
var byte c = 3;
var int ok = _ceu_vector_copy_buffer(&&v, 2, &&c, 8, 1);
escape v[2] + $v + ok;
]],
    run = 6,
}

Test { [[
native/nohold _ceu_vector_copy_buffer;
vector[] byte v = [1,2,0,4,5];
var byte c = 3;
_ceu_vector_copy_buffer(&&v, 2, &&c, 1, 0);
escape v[2] + $v;
]],
    run = 8,
}

Test { [[
native/nohold _ceu_vector_copy_buffer;
vector[5] byte v = [1,2,0,4,5];
var byte c = 3;
var int ok = _ceu_vector_copy_buffer(&&v, 2, &&c, 1, 0);
escape v[2] + $v + ok;
]],
    run = 9,
}

Test { [[
native/nohold _ceu_vector_copy_buffer;
vector[] byte v = [1,2,1,4,5];
var byte c = 3;
var int ok = _ceu_vector_copy_buffer(&&v, 2, &&c, 8, 0);
escape v[2] + $v + ok;
]],
    run = 6,
}

Test { [=[
var float f = 3.2;
var bool ok = [[ 3.1<(@f) and 3.3>(@f) ]];
escape ok;
]=],
    run = 1,
}

Test { [=[
var int f = 3;
var bool ok = [[ 3.0==@f ]];
escape ok;
]=],
    run = 1,
}

Test { [=[
vector[] byte str = [].."12345";
vector[] byte bts = [1,2,3,4,5];
var int r1 = [[ string.len(@str) ]];
var int r2 = [[ string.len(@bts) ]];
escape r1+r2;
]=],
    run = 10,
}

--<<< VECTORS / STRINGS

Test { [[
data SDL_Rect with
    var int x;
end
vector[1] SDL_Rect rcs;
var SDL_Rect ri;
ri = SDL_Rect(10);
rcs[0] = ri;
escape rcs[0].x;
]],
    run = '7] runtime error: access out of bounds',
}

Test { [[
data SDL_Rect with
    var int x;
end
var SDL_Rect ri;
ri = SDL_Rect(10);
vector[1] SDL_Rect rcs = [ri];
escape rcs[0].x;
]],
    run = 10,
}

Test { [[
native/pure _f;
native do
    int f (int* rect) {
        escape *rect;
    }
end

data SDL_Rect with
    var int x;
end
var SDL_Rect ri;
ri = SDL_Rect(10);
vector[1] SDL_Rect rcs = [ri];
escape _f(&&rcs[0] as int&&);
]],
    run = 10,
}

Test { [[
var byte b = 1;
var byte c = 2;
b = (c as byte);
escape b;
]],
    run = 2,
}
Test { [[
vector[] byte c = [1];
vector&[] byte b = &c;
escape b[0];
]],
    run = 1,
}
Test { [[
var byte  c = 2;
var& byte b = &c;
escape b;
]],
    gcc = 'error: pointer targets in assignment differ',
}
Test { [[
var byte   c = 2;
var byte&& b = &&c;
escape *b;
]],
    gcc = 'error: pointer targets in assignment differ',
}
--<< VECTORS

-- STRINGS

Test { [[
native/nohold _strlen;
vector[] byte v = ['a','b','c','\0'];
native _char;
escape _strlen(&&v as _char&&);
]],
    run = 3,
}
Test { [[
native/nohold _strlen;
vector[] byte v = ['a','b','c','\0'];
native _char;
escape _strlen(&&v as _char&&);
]],
    run = 3,
}

Test { [[
native/pure _strlen;
native/nohold _garbage;
native do
    void garbage (byte* v) {
        int i = 0;
        for (; i<20; i++) {
            v[i] = i;
        }
    }
end

vector[10] byte v;
vector[10] byte v_;
native _char;
_garbage(&&v as _char&&);
v = ['a','b','c'];
escape _strlen(&&v as _char&&);
]],
    run = 3,
}

Test { [[
_f([1]);
escape 1;
]],
    --env = 'line 1 : wrong argument #1 : cannot pass plain vectors to native calls',
    --parser = 'line 1 : after `(´ : expected `)´',
    parser = 'line 1 : after `(´ : expected expression',
    --run = 1,
}
Test { [[
_f([1]..[2]);
escape 1;
]],
    --env = 'line 1 : wrong argument #1 : cannot pass plain vectors to native calls',
    --parser = 'line 1 : after `(´ : expected `)´',
    parser = 'line 1 : after `(´ : expected expression',
    --run = 1,
}
Test { [[
vector[] int v;
_f([1]..v);
escape 1;
]],
    --env = 'line 2 : wrong argument #1 : cannot pass plain vectors to native calls',
    parser = 'line 2 : after `(´ : expected expression',
    --parser = 'line 2 : after `(´ : expected `)´',
    --run = 1,
}
Test { [[
vector[] int v;
_f(v..[1]);
escape 1;
]],
    --parser = 'line 2 : after `..´ : invalid constructor syntax',
    parser = 'line 2 : after `v´ : expected `(´ or `[´ or `:´ or `?´ or `!´ or binary operator or `,´ or `)´',
    --run = 1,
}

Test { [[
native/nohold _strlen;
vector[] byte v = "abc";
native _char;
escape _strlen(v as _char&&);
]],
    env = 'line 2 : types mismatch (`byte[]´ <= `_char&&´)',
    --run = 3,
}
Test { [[
native/nohold _strlen;
vector[] byte v = [].."abc";
native _char;
escape _strlen(&&v as _char&&);
]],
    run = 3,
}
Test { [[
native/nohold _strlen;
vector[] byte v = [].."abc";
v = [] .. v .. "def";
native _char;
escape _strlen(&&v as _char&&);
]],
    run = 6,
}

Test { [[
var int nnn = 10;
vector[nnn] u8 xxx;
xxx[0] = 10;
escape 1;
]],
    run = ':3] runtime error: access out of bounds',
}

Test { [[
var int nnn = 10;
vector[nnn] u8 xxx;
$xxx := nnn;
xxx[0] = 10;
xxx[9] = 1;
escape xxx[0]+xxx[9];
]],
    run = 11,
}

Test { [[
var int nnn = 10;
vector[nnn] u8 xxx;
$xxx := nnn+1;
escape 1;
]],
    run = ':3] runtime error: invalid attribution : out of bounds',
}

Test { [[
var int n = 10;
vector[n] u8 us;
$us := n;
$us = 20;
escape 1;
]],
    run = ':4] runtime error: invalid attribution : out of bounds',
}

Test { [[
var int n = 10;
vector[] u8 us;
$us = n;
escape 1;
]],
    run = ':3] runtime error: invalid attribution : out of bounds',
}

Test { [[
var int n = 10;
vector[] u8 us;
$us := n;
escape 1;
]],
    run = 1,
}

Test { [[
var int n = 10;
vector[n] u8 us = [0,1,2,3,4,5,6,7,8,9];
us[n] = 10;
escape us[0]+us[9];
]],
    run = ':3] runtime error: access out of bounds',
}

Test { [[
var int n = 10;
vector[n] u8 us = [0,1,2,3,4,5,6,7,8,9];
us[n-1] = 1;
escape us[0]+us[9];
]],
    run = 1,
}


Test { [[
native _u8;
var int n = 10;
var _u8[n] us = [];
us[0] = 10;
us[9] = 1;
escape us[0]+us[9];
]],
    env = 'line 2 : dimension must be constant',
}

Test { [[
native _u8, _U8_MAX;
var int n = 10;
var _u8[_U8_MAX] us = [];
us[_U8_MAX-1] = 10;
us[0] = 1;
escape us[0]+us[_U8_MAX-1];
]],
    env = 'line 2 : dimension must be constant',
}

Test { [[
native _u8;
native/const _U8_MAX;
var int n = 10;
var _u8[_U8_MAX] us = [];
us[_U8_MAX-1] = 10;
us[0] = 1;
escape us[0]+us[_U8_MAX-1];
]],
    run = 11,
}

Test { [[
native _u8, _N;
pre native do
    int N = 10;
end
var int n = 10;
var _u8[_N] us = [];
us[_N-1] = 10;
us[0] = 1;
escape us[0]+us[_N-1];
]],
    env = 'line 5 : dimension must be constant',
}

Test { [[
native _u8;
native/const _N;
pre native do
    int N = 10;
end
var int n = 10;
var _u8[_N] xxxx = [];
xxxx[_N-1] = 10;
xxxx[0] = 1;
escape xxxx[0]+xxxx[_N-1];
]],
    gcc = '6:26: error: variably modified ‘xxxx’ at file scope',
}

Test { [[
#define HASH_BYTES 32
vector[HASH_BYTES+sizeof(u32)] byte bs;
escape $$bs;
]],
    run = 36,
}

Test { [[
var int n = 32;
vector[n] byte bs;
escape $$bs;
]],
    run = 32,
}

Test { [[
code/instantaneous Fx (void)=>void do
    var int x = 0;

    vector[10] byte cs;
end
escape 1;
]],
    wrn = true,
    props = 'line 4 : not permitted inside `function´',
}

Test { [[
code/instantaneous Fx (vector&[] byte cs)=>void do
    cs[0] = 10;
end
vector[] byte cs = [0];
Fx(&cs);
escape cs[0];
]],
    run = 10,
}

--<<< VECTORS / STRINGS

    -- NATIVE C FUNCS BLOCK RAW

Test { [[
native _char;
var _char c = 1;
escape c;
]],
    run = 1,
}

Test { [[
native/plain _int;
var _int a=1, b=1;
a = b;
await 1s;
escape a==b;
]],
    run = { ['~>1s'] = 1 },
}

Test { [[
var int a=1, b=1;
a = b;
await 1s;
escape a==b;
]],
    run = { ['~>1s'] = 1 },
}

Test { [[
escape {1};
]],
    run = 1,
}

Test { [[
native _V;
{ int V = 10; };
escape _V;
]],
    run = 10,
}

Test { [[
{
    static int v;
    if (0) {
    } else {
        v = 1;
    }
};
escape {v};
]],
    run = 1,
}

Test { [[
var& void? p;
do p = &{ NULL };
finalize with
    nothing;
end
escape p! ==null;
]],
    env = 'line 7 : invalid operands to binary "=="',
    --run = 1,
}

Test { [[
var& void? p;
p := { NULL };
escape 1;
//escape p==null;
]],
    env = 'line 2 : invalid attribution : missing `!´ (in the left) or `&´ (in the right)',
    --ref = 'line 2 : invalid attribution : missing alias operator `&´',
    --fin = 'line 2 : attribution requires `finalize´',
}

Test { [[
_f()
]],
    parser = 'line 1 : after `)´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `=´ or `:=´ or `;´',
}

Test { [[
native _printf;
do
    _printf("oi\n");
end
escape 10;
]],
    run = 10;
}

Test { [[
native _V;
native do
    int V[2][2] = { {1, 2}, {3, 4} };
end

_V[0][1] = 5;
escape _V[1][0] + _V[0][1];
]],
    run = 8,
}

Test { [[
native _END;
native do
    int END = 1;
end
if not  (_END-1==1) then
    escape 1;
else
    escape 0;
end
]],
    run = 1,
}

Test { [[
native do
end
escape 1;
]],
    run = 1,
}

Test { [[
native do
    byte* a = "end";
end
escape 1;
]],
    run = 1,
}

Test { [[
native do
    /*** END ***/
    byte* a = "end";
    /*** END ***/
end
escape 1;
]],
    run = 1,
}

Test { [[
native do
    int A () {}
end
A = 1;
escape 1;
]],
    --adj = 'line 4 : invalid expression',
    --parser = 'line 3 : after `end´ : expected statement'
    parser = 'line 3 : after `end´ : expected `;´ or statement',
    --parser = 'line 4 : after `A´ : expected `(´',
}

Test { [[
native do
    void A (int v) {}
end
escape 0;
]],
    run = 0;
}

Test { [[
native do
    int A (int v) { escape 1; }
end
escape 0;
]],
    --env = 'A : incompatible with function definition',
    run = 0,
}

Test { [[
native _A;
native do
    void A (int v) {}
end
_A();
escape 0;
]],
    --env = 'line 5 : native function "_A" is not declared',
    --run  = 1,
    gcc = 'error: too few arguments to function ‘A’',
}

Test { [[
native _A;
native do
    void A (int v) { }
end
_A(1);
escape 0;
]],
    run = 0,
}

Test { [[
native _A;
native do
    void A () {}
end
var int v = _A();
escape v;
]],
    gcc = 'error: void value not ignored as it ought to be',
}

Test { [[emit A => 10; escape 0;]],
    tops = 'external "A" is not declared'
}

Test { [[
native _Const;
native do
    int Const () {
        escape -10;
    }
end
var int ret = _Const();
escape ret;
]],
    run = -10
}

Test { [[
native _ID;
native do
    int ID (int v) {
        escape v;
    }
end
escape _ID(10);
]],
    run = 10,
}

Test { [[
native _ID;
native do
    int ID (int v) {
        escape v;
    }
end
var int v = _ID(10);
escape v;
]],
    run = 10
}

Test { [[
native _VD;
native do
    void VD (int v) {
    }
end
_VD(10);
escape 1;
]],
    run = 1
}

Test { [[
native _VD;
native do
    void VD (int v) {
    }
end
var int ret = _VD(10);
escape ret;
]],
    gcc = 'error: void value not ignored as it ought to be',
}

Test { [[
native _VD;
native do
    void VD (int v) {
    }
end
var void v = _VD(10);
escape v;
]],
    env = 'line 5 : cannot instantiate type "void"',
}

Test { [[
native _NEG;
native do
    int NEG (int v) {
        escape -v;
    }
end
escape _NEG(10);
]],
    run = -10,
}

Test { [[
native _NEG;
native do
    int NEG (int v) {
        escape -v;
    }
end
var int v = _NEG(10);
escape v;
]],
    run = -10
}

Test { [[
native _ID;
native do
    int ID (int v) {
        escape v;
    }
end
input int A;
var int v=0;
par/and do
    await A;
with
    v = _ID(10);
end;
escape v;
]],
    run = {['1~>A']=10},
}

Test { [[
native _ID;
native do
    int ID (int v) {
        escape v;
    }
end
input int A;
var int v=0;
par/or do
    await A;
with
    v = _ID(10);
end
escape v;
]],
    _ana = {
        unreachs = 1,
    },
    run = 10,
}

Test { [[
native _Z1;
native do int Z1 (int a) { escape a; } end
input int A;
var int c;
_Z1(3);
c = await A;
escape c;
]],
    run = {
        ['10~>A ; 20~>A'] = 10,
        ['3~>A ; 0~>A'] = 3,
    }
}

Test { [[
native/nohold _f1, _f2;
native do
    int f1 (u8* v) {
        escape v[0]+v[1];
    }
    int f2 (u8* v1, u8* v2) {
        escape *v1+*v2;
    }
end
native _u8;
var _u8[2] v = [];
v[0] = 8;
v[1] = 5;
escape _f2(&&v[0],&&v[1]) + _f1(&&v) + _f1(&&v[0]);
]],
    run = 39,
}

Test { [[
native do
    void* V;
end
var void&& v = null;
native _V;
_V = v;
escape 1;
]],
    run = 1,
}

Test { [[
native do
    void* V;
end
var void&& v=null;
native _V;
_V = v;
await 1s;
escape _V==null;
]],
    run = false,
    --fin = 'line 7 : pointer access across `await´',
}

Test { [[
do
    var int&& p, p1;
    input int&& E;
    p = await E;
    p1 = p;
    await E;
    escape *p1;
end
]],
    --run = 1,
    fin = 'line 7 : unsafe access to pointer "p1" across `await´',
}

Test { [[
native _tp, _a, _b;
native do
    typedef int tp;
end
var _tp&& v=null;
_a = v;
await 1s;
_b = _a;    // _a pode ter escopo menor e nao reclama de FIN
await FOREVER;
]],
    --fin = 'line 7 : pointer access across `await´',
    _ana = {
        isForever = true,
    },
}

Test { [[
var int v = 10;
var int&& x = &&v;
event void e;
var int ret=0;
if true then
    ret = *x;
    emit e;
else
    emit e;
    escape *x;
end
escape ret;
]],
    fin = 'line 10 : unsafe access to pointer "x" across `emit´',
}

Test { [[
var int v = 10;
var int&& x = &&v;
event void e;
var int ret=0;
if true then
    ret = *x;
    emit e;
else
    escape *x;
end
escape ret;
]],
    run = 10,
}

Test { [[
var int v = 10;
var int&& x = &&v;
event void e;
var int ret=0;
par do
    ret = *x;
    emit e;
with
    escape *x;
end
]],
    _ana = {acc=2},
    run = 10,
}

Test { [[
var int v = 10;
var int&& x = &&v;
event void e;
var int ret=0;
par do
    ret = *x;
    emit e;
with
    par/or do
        ret = *x;
        emit e;
    with
        ret = *x;
        await e;
    with
        par/and do
            ret = *x;
            emit e;
        with
            ret = *x;
            await e;
        end
    end
with
    escape *x;
end
]],
    _ana = {acc=true},
    run = 10,
}

Test { [[
native/plain _SDL_Rect, _SDL_Point;
var _SDL_Point pos;

var _SDL_Rect rect = _SDL_Rect(pos.x, pos.y);
await 1s;
var _SDL_Rect r = rect;
escape 1;
]],
    ref = 'line 4 : invalid access to uninitialized variable "pos" (declared at tests.lua:2)',
}
Test { [[
native/plain _SDL_Rect, _SDL_Point;
var _SDL_Point pos = _SDL_Point(1,1);

var _SDL_Rect rect = _SDL_Rect(pos.x, pos.y);
await 1s;
var _SDL_Rect r = rect;
escape 1;
]],
    gcc = 'error: unknown type name ‘SDL_Point’',
}

Test { [[
native/plain _SDL_Rect, _SDL_Point;
var _SDL_Point pos = _SDL_Point(1,1);

var _SDL_Rect rect = _SDL_Rect(pos.x, pos.y);
await 1s;
var _SDL_Rect r = rect;
    r.x = r.x - r.w/2;
    r.y = r.y - r.h/2;
escape 1;
]],
    gcc = 'error: unknown type name ‘SDL_Point’',
}

Test { [[
native _int, _f;
native do
    int f () {
        escape 1;
    }
end
var _int[2] v = [];
v[0] = 0;
v[1] = 1;
v[_f()] = 2;
escape v[1];
]],
    run = 2,
}

Test { [[
var int xxx = 10;
escape ((__ceu_app:data as _CEU_Main&&)):xxx;
]],
    parser = 'line 2 : after `:´ : expected internal identifier or native identifier',
}
Test { [[
native __ceu_app, _CEU_Main;
var int xxx = 10;
escape ((__ceu_app:_data as _CEU_Main&&)):xxx;
]],
    run = 10,
}
-- NATIVE/PRE

Test { [[
pre native do
    typedef struct {
        int a,b,c;
    } Fx;
end
native _Fx, _fff;
native do
    Fx* fff;
end

native ___ceu_nothing;
input OPEN  (var byte&& path, var  byte&& mode)=>_Fx&& do
    ___ceu_nothing(path);
    ___ceu_nothing(mode);
    escape _fff;
end

input CLOSE  (var _Fx&& f)=>int do
    ___ceu_nothing(f);
    escape 1;
end

input SIZE  (var _Fx&& f)=>int do
    ___ceu_nothing(f);
    escape 1;
end

input READ  (var void&& ptr, var int size, var int nmemb, var  _Fx&& f)=>int do
    ___ceu_nothing(ptr);
    ___ceu_nothing(&&size);
    ___ceu_nothing(&&nmemb);
    ___ceu_nothing(f);
    escape 1;
end

escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
pre native do
    typedef struct {
        byte* str;
        u32   length;
        u32   x;
        u32   y;
    } draw_string_t;
end

native ___ceu_nothing;
native/plain _draw_string_t;
input DRAW_STRING  (var _draw_string_t&& ptr)=>void do
    ___ceu_nothing(ptr);
end

var _draw_string_t v = _draw_string_t(
    "Welcome to Ceu/OS!\n",
    20,
    100,
    100);
call DRAW_STRING => &&v;

escape 1;
]],
    run = 1,
}

--[=[

PRE = [[
native do
    static inline int idx (@const int* vec, int i) {
        escape vec[i];
    }
    static inline int set (int* vec, int i, int val) {
        vec[i] = val;
        escape val;
    }
end
@pure _idx;
int[2] va;

]]

Test { PRE .. [[
_set(va,1,1);
escape _idx(va,1);
]],
    run = 1,
}

Test { PRE .. [[
_set(va,0,1);
_set(va,1,2);
escape _idx(va,0) + _idx(va,1);
]],
    run = 3,
}

Test { PRE .. [[
par/and do
    _set(va,0,1);
with
    _set(va,1,2);
end;
escape _idx(va,0) + _idx(va,1);
]],
    _ana = {
        acc = 2,
    },
}
Test { PRE .. [[
par/and do
    _set(va,0,1);
with
    _idx(va,1);
end;
escape _idx(va,0) + _idx(va,1);
]],
    _ana = {
        acc = 1,
    },
}
Test { PRE .. [[
_set(va,0,1);
_set(va,1,2);
par/and do
    _idx(va,0);
with
    _idx(va,1);
end;
escape _idx(va,0) + _idx(va,1);
]],
    run = 3,
}

Test { [[
int a, b;
int&& pa, pb;

par/or do
    pa = &&a;
with
    pb = &&b;
end;
escape 1;
]],
    run = 1
}

PRE = [[
@pure _f3, _f5;
native do
int f1 (int* a, int* b) {
    escape *a + *b;
}
int f2 (@const int* a, int* b) {
    escape *a + *b;
}
int f3 (@const int* a, const int* b) {
    escape *a + *b;
}
int f4 (int* a) {
    escape *a;
}
int f5 (@const int* a) {
    escape *a;
}
end
]]

Test { PRE .. [[
int a = 1;
int b = 2;
escape _f1(&&a,&&b);
]],
    run = 3,
}

Test { PRE .. [[
int&& pa;
par/or do
    _f4(pa);
with
    int v = 1;
end;
escape 0;
]],
    _ana = {
        acc = 1,
    },
}
Test { PRE .. [[
int a;
par/or do
    _f4(&&a);
with
    int v = a;
end;
escape 0;
]],
    _ana = {
        acc = 1,
    },
}
Test { PRE .. [[
int a, b;
par/or do
    _f5(&&a);
with
    a = 1;
end;
escape 0;
]],
    _ana = {
        acc = 1,
    },
}
Test { PRE .. [[
int a = 10;
par/or do
    _f5(&&a);
with
    escape a;
end;
escape 0;
]],
    run = false,
    _ana = {
        --abrt = 1,
    }
}
Test { PRE .. [[
int a;
int&& pa;
par/or do
    _f5(pa);
with
    escape a;
end;
escape 0;
]],
    --abrt = 1,
    _ana = {
        acc = 1,
    },
}
Test { PRE .. [[
int a, b;
par/or do
    _f4(&&a);
with
    int v = b;
end;
escape 0;
]],
    run = 0,
}
Test { PRE .. [[
int a, b;
par/or do
    _f5(&&a);
with
    b = 1;
end;
escape 0;
]],
    run = 0,
}

Test { PRE .. [[
int a, b;
par/or do
    _f5(&&a);
with
    int v = b;
end;
escape 0;
]],
    run = 0,
}
Test { PRE .. [[
int&& pa;
do
    int a;
    pa = &&a;
end;
escape 1;
]],
    run = 1,     -- TODO: check_depth
    --env = 'invalid attribution',
}
Test { PRE .. [[
int a=1;
do
    int&& pa = &&a;
    *pa = 2;
end;
escape a;
]],
    run = 2,
}

Test { PRE .. [[
int a;
int&& pa;
par/or do
    _f4(pa);
with
    int v = a;
end;
escape 0;
]],
    _ana = {
        acc = 2, -- TODO: scope of v vs pa
    },
}
Test { PRE .. [[
int a;
int&& pa;
par/or do
    _f5(pa);
with
    a = 1;
end;
escape a;
]],
    _ana = {
        acc = 1,
    },
}
Test { PRE .. [[
int a;
int&& pa;
par do
    escape _f5(pa);
with
    escape a;
end;
]],
    --abrt = 2,
    _ana = {
        acc = 2, -- TODO: $ret vs anything is DET
    },
}

Test { PRE .. [[
int a=1, b=5;
par/or do
    _f4(&&a);
with
    _f4(&&b);
end;
escape a+b;
]],
    _ana = {
        acc = 1,
    },
    run = 6,
}

Test { PRE .. [[
int a = 1;
int b = 2;
int v1, v2;
par/and do
    v1 = _f1(&&a,&&b);
with
    v2 = _f1(&&a,&&b);
end;
escape v1 + v2;
]],
    _ana = {
        acc = 3,
    },
}

Test { PRE .. [[
int a = 1;
int b = 2;
int v1, v2;
par/and do
    v1 = _f2(&&a,&&b);
with
    v2 = _f2(&&a,&&b);
end;
escape v1 + v2;
]],
    _ana = {
        acc = 3,     -- TODO: f2 is const
    },
}

Test { PRE .. [[
int a = 1;
int b = 2;
int v1, v2;
par/and do
    v1 = _f3(&&a,&&b);
with
    v2 = _f3(&&a,&&b);
end;
escape v1 + v2;
]],
    run = 6,
}

Test { PRE .. [[
int a = 2;
int b = 2;
int v1, v2;
par/and do
    v1 = _f4(&&a);
with
    v2 = _f4(&&b);
end;
escape a+b;
]],
    run = 4,
    _ana = {
        acc = 1,
    },
}

Test { PRE .. [[
int a = 2;
int b = 2;
int v1, v2;
par/and do
    v1 = _f4(&&a);
with
    v2 = _f4(&&a);
end;
escape a+a;
]],
    _ana = {
        acc = 2,
    },
}

Test { PRE .. [[
int a = 2;
int b = 2;
int v1, v2;
par/and do
    v1 = _f5(&&a);
with
    v2 = _f5(&&a);
end;
escape a+a;
]],
    run = 4,
}

Test { PRE .. [[
int a;
int&& pa = &&a;
a = 2;
int v1,v2;
par/and do
    v1 = _f4(&&a);
with
    v2 = _f4(pa);
end;
escape v1+v2;
]],
    _ana = {
        acc = 3,
    },
}

Test { PRE .. [[
int a;
int&& pa = &&a;
a = 2;
int v1,v2;
par/and do
    v1 = _f5(&&a);
with
    v2 = _f5(pa);
end;
escape v1+v2;
]],
    _ana = {
        acc = 1,
    },
}

Test { [[
par/and do
    _printf("END: 1\n");
with
    _assert(1);
end
escape 0;
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}

Test { [[
deterministic _printf with _assert;
native do ##include <assert.h> end
par/and do
    _printf("END: 1\n");
with
    _assert(1);
end
escape 0;
]],
    todo = true,
    run = 1,
}
--]=]

Test { [[
native _a;
native do
    int a;
end
par/or do
    _a = 1;
with
    _a = 2;
end
escape _a;
]],
    _ana = {
        acc = 1,
        abrt = 1,
    },
}

Test { [[
@const _HIGH, _LOW;
par do
    loop do
        _digitalWrite(11, _HIGH);
        await 1s;
        _digitalWrite(11, _LOW);
        await 1s;
    end
with
    loop do
        _digitalWrite(12, _HIGH);
        await 500ms;
        _digitalWrite(12, _LOW);
        await 500ms;
    end
with
    loop do
        _digitalWrite(13, _HIGH);
        await 250ms;
        _digitalWrite(13, _LOW);
        await 250ms;
    end
end
]],
    todo = true,
    _ana = {
        acc = 6,
        isForever = true,
    },
}

Test { [[
native _LOW, _HIGH, _digitalWrite;
par do
    loop do
        _digitalWrite(11, _HIGH);
        await 1s;
        _digitalWrite(11, _LOW);
        await 1s;
    end
with
    loop do
        _digitalWrite(12, _HIGH);
        await 500ms;
        _digitalWrite(12, _LOW);
        await 500ms;
    end
with
    loop do
        _digitalWrite(13, _HIGH);
        await 250ms;
        _digitalWrite(13, _LOW);
        await 250ms;
    end
end
]],
    _ana = {
        acc = true,
        isForever = true,
    },
    --fin = 'line 4 : call requires `finalize´',
}

Test { [[
native/const _LOW, _HIGH;
native _digitalWrite;
par do
    loop do
        _digitalWrite(11, _HIGH);
        await 1s;
        _digitalWrite(11, _LOW);
        await 1s;
    end
with
    loop do
        _digitalWrite(12, _HIGH);
        await 500ms;
        _digitalWrite(12, _LOW);
        await 500ms;
    end
with
    loop do
        _digitalWrite(13, _HIGH);
        await 250ms;
        _digitalWrite(13, _LOW);
        await 250ms;
    end
end
]],
    _ana = {
        acc = true,
        isForever = true,
    },
}

    -- RAW

Test { [[
native do
    int V = 0;
    int fff (int a, int b) {
        V = V + a + b;
        escape V;
    }
end
{fff}(1,2);
var int i = {fff}(3,4);
escape i;
]],
    parser = 'line 8 : after `)´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `=´ or `:=´',
}

Test { [[
native do
    int V = 0;
    int fff (int a, int b) {
        V = V + a + b;
        escape V;
    }
end
call {fff}(1,2);
var int i = {fff}(3,4);
escape i;
]],
    run = 10,
}

    -- STRINGS

Test { [[
native _char;
var _char[10] a;
a = "oioioi";
escape 1;
]],
    gcc = '2:32: error: assignment to expression with array type',
    --env = 'line 2 : types mismatch (`_char[]´ <= `_char&&´)',
    --env = 'line 2 : invalid attribution',
}

Test { [[
var byte&& a;
a = "oioioi";
escape 1;
]],
    run = 1,
}

Test { [[
var int a;
a = "oioioi";
escape 1;
]],
    env = 'line 2 : types mismatch (`int´ <= `_char&&´)',
}

Test { [[
native _char;
var _char&& a = "Abcd12" ;
escape 1;
]],
    --env = 'line 2 : invalid attribution (_char&& vs byte&&)',
    run = 1,
}
Test { [[
native _char;
var _char&& a = ("Abcd12"  as _char&&);
escape 1;
]],
    run = 1
}
Test { [[
native _printf;
_printf("END: %s\n", "Abcd12");
escape 0;
]],
    run='Abcd12',
}
Test { [[
native _strlen;
escape _strlen("123");
]], run=3 }
Test { [[
native _printf;
_printf("END: 1%d 0\n",2); escape 0;]], run=12 }
Test { [[
native _printf;
_printf("END: 1%d%d 0\n",2,3); escape 0;]], run=123 }

Test { [[
native/nohold _strncpy, _printf, _strlen;
native _char ;
var _char[10] str = [];
_strncpy(&&str, "123", 4);
_printf("END: %d %s\n", _strlen(&&str) as int, &&str);
escape 0;
]],
    run = '3 123'
}

Test { [[
native/nohold _printf, _strlen, _strcpy;
native _char;
var _char[6] a=[]; _strcpy(&&a, "Hello");
var _char[2] b=[]; _strcpy(&&b, " ");
var _char[7] c=[]; _strcpy(&&c, "World!");
var _char[30] d=[];

var int len = 0;
_strcpy(&&d,&&a);
_strcpy(&&d[_strlen(&&d)], &&b);
_strcpy(&&d[_strlen(&&d)], &&c);
_printf("END: %d %s\n", _strlen(&&d) as int, &&d);
escape 0;
]],
    run = '12 Hello World!'
}

Test { [[
native _const_1;
native do
    int const_1 () {
        escape 1;
    }
end
escape _const_1();
]],
    run = 1;
}

Test { [[
native _const_1;
native do
    int const_1 () {
        escape 1;
    }
end
escape _const_1() + _const_1();
]],
    run = 2;
}

Test { [[
native _inv;
native do
    int inv (int v) {
        escape -v;
    }
end
var int a;
a = _inv(_inv(1));
escape a;
]],
    --fin = 'line 8 : call requires `finalize´',
    run = 1,
}

Test { [[
native/pure _inv;
native do
    int inv (int v) {
        escape -v;
    }
end
var int a;
a = _inv(_inv(1));
escape a;
]],
    run = 1,
}

Test { [[
native _id;
native do
    int id (int v) {
        escape v;
    }
end
var int a;
a = _id(1);
escape a;
]],
    run = 1
}

Test { [[
vector[2] int v;
par/or do
    v[0] = 1;
with
    v[1] = 2;
end;
escape 0;
]],
    _ana = {
        acc = 1,
        abrt = 1,
    },
}
Test { [[
vector[2] int v;
var int i=0,j=0;
par/or do
    v[j] = 1;
with
    v[i+1] = 2;
end;
escape 0;
]],
    _ana = {
        acc = 1,
        abrt = 1,
    },
}

-- STRUCTS / SIZEOF

Test { [[
pre native do
typedef struct {
    u16 a;
    u8 b;
    u8 c;
} s;
end
native/plain _s;
var _s vs = _s(10,1,0);
escape vs.a + vs.b + sizeof(_s);
]],
    run = 15,
}

Test { [[
pre native do
typedef struct {
    u16 a;
    u8 b;
    u8 c;
} s;
end
native/plain _s;
var _s vs = _s(10,1,0);
escape vs.a + vs.b + sizeof(_s) + sizeof(vs) + sizeof(vs.a);
]],
    run = 21,
}

Test { [[
native _SZ;
native _aaa = (sizeof<void&&,u16>) * 2;
native do
    typedef struct {
        void&& a;
        u16 b;
    } t1;
    typedef struct {
        t1 v[2];
    } aaa;
    int SZ = sizeof(aaa);
end
escape sizeof<_aaa> + _SZ;
]],
    todo = 'sizeof',
    run = 28,   -- TODO: different packings
}

Test { [[
pre native do
    typedef struct {
        u16 ack;
        u8 data[16];
    } Payload;
end
native _Payload ;
var _Payload final;
var u8&& neighs = &&(final._data[4]);
escape 1;
]],
    ref = 'line 9 : invalid access to uninitialized variable "final" (declared at tests.lua:8)',
}
Test { [[
pre native do
    typedef struct {
        u16 ack;
        u8 data[16];
    } Payload;
end
native/plain _Payload;
var _Payload final = _Payload(0,{});
var u8&& neighs = &&(final._data[4]);
escape 1;
]],
    run = 1;
}

Test { [[
native do
typedef struct {
    int a;
    int b;
} s;
end
native/plain _s;
var _s vs = _s(0,0);
par/and do
    vs.a = 10;
with
    vs.a = 1;
end;
escape vs.a;
]],
    _ana = {
        acc = 1,
    },
}

Test { [[
native do
typedef struct {
    int a;
    int b;
} s;
end
native/plain _s;
var _s vs = _s(0,0);
par/and do
    vs.a = 10;
with
    vs.b = 1;
end;
escape vs.a;
]],
    _ana = {
        acc = 1,     -- TODO: struct
    },
}

Test { [[
pre native do
    typedef struct {
        int a;
    } mys;
end
native/plain _mys;
var _mys v = _mys(0);
var _mys&& pv;
pv = &&v;
v.a = 10;
(*pv).a = 20;
pv:a = pv:a + v.a;
escape v.a;
]],
    run = 40,
}

Test { [[
]],
    _ana = {
        reachs = 1,
        isForever = true,
    }
}

Test { [[
native/plain _char;
native _u8;
var _u8[10] v1 = [];
var _char[10] v2 = [];

loop i in [0 |> 10[ do
    v1[i] = i;
    v2[i] = ((i*2) as _char);
end

var int ret = 0;
loop i in [0 |> 10[ do
    ret = ret + (v2[i] as u8) - v1[i];
end

escape ret;
]],
    --loop = 1,
    run = 45,
}

Test { [[
native _message_t ;
native _t = sizeof<_message_t, u8>;
escape sizeof<_t>;
]],
    todo = 'sizeof',
    run = 53,
}

Test { [[
native _char;
var _char a = (1 as _char);
escape a as int;
]],
    run = 1,
}

-- Exps

Test { [[var int a = ]],
    parser = "line 1 : after `=´ : expected expression",
}

Test { [[escape]],
    parser = "line 1 : after `escape´ : expected expression",
}

Test { [[escape()]],
    --parser = "line 1 : after `(´ : expected expression",
    parser = "line 1 : after `(´ : expected expression",
}

Test { [[escape 1+;]],
    parser = "line 1 : after `+´ : expected expression",
}

Test { [[if then]],
    parser = "line 1 : after `if´ : expected expression",
}

Test { [[b = ;]],
    parser = "line 1 : after `=´ : expected expression",
}


Test { [[


escape 1

+


;
]],
    parser = "line 5 : after `+´ : expected expression"
}

Test { [[
var int a;
a = do/_
    var int b;
end
]],
    parser = "line 4 : after `end´ : expected `;´",
}

    -- POINTER ASSIGNMENTS

Test { [[
var int&& x;
*x = 1;
escape 1;
]],
    ref = 'line 2 : invalid access to uninitialized variable "x" (declared at tests.lua:1)',
}

Test { [[
var int&& p;
do
    var int i;
    p = &&i;
end
escape 1;
]],
    ref = 'line 1 : uninitialized variable "p" crossing compound statement (tests.lua:2)',
    fin = 'line 4 : attribution to pointer with greater scope',
}
Test { [[
var int&& p=null;
do
    var int i=0;
    p := &&i;
end
escape 1;
]],
    run = 1,
}
Test { [[
var int a := 1;
escape 1;
]],
    fin = 'line 1 : wrong operator',
}
Test { [[
var int a;
a := 1;
escape 1;
]],
    fin = 'line 2 : wrong operator',
}
Test { [[
var int a;
do a = 1;
finalize with
    nothing;
end
escape 1;
]],
    fin = 'line 3 : attribution does not require `finalize´',
}
Test { [[
var int&& a := null;
escape 1;
]],
    fin = 'line 1 : wrong operator',
}
Test { [[
var int&& a;
a := null;
escape 1;
]],
    fin = 'line 2 : wrong operator',
}
Test { [[
var int&& a;
do a = null;
finalize with
    nothing;
end
escape 1;
]],
    fin = 'line 3 : attribution does not require `finalize´',
}
Test { [[
code/instantaneous Faca (void)=>void do
    var int&& a;
    a := null;
end
escape 1;
]],
    wrn = true,
    fin = 'line 3 : wrong operator',
}
Test { [[
var int a=0;
var int&& pa := &&a;
escape 1;
]],
    fin = 'line 2 : wrong operator',
    run = 1,
}
Test { [[
var int a=0;
var int&& pa;
do pa = &&a;
finalize with
    nothing;
end
escape 1;
]],
    fin = 'line 4 : attribution does not require `finalize´',
}
Test { [[
code/instantaneous Fx (var void&& o1)=>void do
    var void&& tmp := o1;
end
escape 1;
]],
    wrn = true,
    fin = 'line 2 : wrong operator',
    --fin = 'line 2 : pointer access across `await´',
}

Test { [[
native _int;
var _int&& u;
var _int[1] i;
await 1s;
u = i;
escape 1;
]],
    env = 'line 4 : types mismatch (`_int&&´ <= `_int[]´)',
    --run = { ['~>1s']=1 },
}
Test { [[
native _int;
var _int&& u;
var _int[1] i=[];
await 1s;
u = &&i[0];
escape 1;
]],
    fin = 'line 4 : unsafe access to pointer "i" across `await´ (tests.lua : 3)',
}
Test { [[
native/plain _int;
var _int&& u;
var _int[1] i=[];
await 1s;
u = &&i[0];
if u==null then end;
escape 1;
]],
    run = { ['~>1s']=1 },
}
Test { [[
var int&& u;
vector[1] int i;
await 1s;
u = i;
escape 1;
]],
    env = 'line 4 : types mismatch (`int&&´ <= `int[]´)',
    --run = { ['~>1s']=1 },
}
Test { [[
native _int;
var _int&& u;
do
    var _int[1] i;
    i[0] = 2;
    u = i;
end
do
    var _int[1] i;
    i[0] = 5;
end
escape *u;
]],
    env = 'line 5 : types mismatch (`_int&&´ <= `_int[]´)',
}
Test { [[
native _int;
var _int&& u;
do
    var _int[1] i;
    i[0] = 2;
    u = &&i[0];
end
do
    var _int[1] i;
    i[0] = 5;
end
escape *u;
]],
    ref = 'line 1 : uninitialized variable "u" crossing compound statement (tests.lua:2)',
    fin = 'line 5 : attribution to pointer with greater scope',
}
Test { [[
input int SDL_KEYUP;
var int key;
key = await SDL_KEYUP;
escape key;
]],
    run = { ['1 ~> SDL_KEYUP']=1 }
}

Test { [[
input int&& SDL_KEYUP;
par/or do
    var int&& key;
    key = await SDL_KEYUP;
    if key==null then end;
with
    async do
        emit SDL_KEYUP => null;
    end
end
escape 1;
]],
    run = 1.
}

Test { [[
native _SDL_KeyboardEvent;
input _SDL_KeyboardEvent&& SDL_KEYUP;
every key in SDL_KEYUP do
    if key:keysym.sym == 1 then
    else/if key:keysym.sym == 1 then
    end
end
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
code/instantaneous GetVS (var void&& && o1, var  void&& && o2)=>int do
    if (*o1) then
        escape 1;
    else/if (*o2) then
        var void&& tmp = *o1;
        *o1 = *o2;
        *o2 := tmp;
            // tmp is an alias to "o1"
        escape 1;
    else
        //*o1 = NULL;
        //*o2 = NULL;
        escape 0;
    end
end
escape 1;
]],
    wrn = true,
    run = 1,
}

    -- CPP / DEFINE / PREPROCESSOR

Test { [[
var u8 cnt;
vector[3] u8 v;

v = [] .. v .. [17];
v = [] .. v .. [9];

cnt = #v;
_printf("oi\n");
escape cnt;
]],
    parser = 'line 7 : after `=´ : expected expression',
}

Test { [[
#define _OBJ_N + 2
vector[_OBJ_N] void&& objs;
escape 1;
]],
    run = 1,
}

Test { [[
#define _OBJ_N + 2 \
               + 1
vector[_OBJ_N] void&& objs;
escape 1;
]],
    run = 1,
}

Test { [[
#define OI

a = 1;
]],
    locs = 'line 3 : internal identifier "a" is not declared',
}

Test { [[
native/const _N;
pre native do
    #define N 1
end
native _u8;
var _u8[_N] vec = [];
vec[0] = 10;
escape vec[_N-1];
]],
    run = 10,
}

Test { [[
pre native do
    #define N 1
end
native _u8;
var _u8[N] vec = [];
vec[0] = 10;
escape vec[N-1];
]],
    run = 10,
}

Test { [[
pre native do
    #define N 1
end
native _u8;
var _u8[N+1] vec = [];
vec[1] = 10;
escape vec[1];
]],
    run = 10,
}

Test { [[
#define N 1
native _u8;
var _u8[N+1] vec = [];
vec[1] = 10;
escape vec[1];
]],
    run = 10,
}

Test { [[
native/const _N;
pre native do
    #define N 5
end
native _int;
var _int[_N] vec = [];
loop i in [0 |> _N[ do
    vec[i] = i;
end
var int ret = 0;
loop i in [0 |> _N[ do
    ret = ret + vec[i];
end
escape ret;
]],
    --loop = true,
    wrn = true,
    run = 10,
}

Test { [[
#define UART0_BASE 0x20201000
#define UART0_CR ((UART0_BASE + 0x30) as u32&&)
*UART0_CR = 0x00000000;
escape 1;
]],
    valgrind = false,
    asr = true,
}
-- ASYNC

Test { [[
input void A;
par/or do
    async do
        emit A;
    end
    escape -1;
with
    await A;
end
async do
end
escape 1;
]],
    run = 1,
}

Test { [[
async do

    par/or do
        var int a=0;
    with
        var int b=0;
    end
end
]],
    props = "line 3 : not permitted inside `async´",
}
Test { [[
async do


    par/and do
        var int a=0;
    with
        var int b=0;
    end
end
]],
    props = "line 4 : not permitted inside `async´",
}
Test { [[
async do
    par do
        var int a=0;
    with
        var int b=0;
    end
end
]],
    props = "line 2 : not permitted inside `async´",
}

-- DFA

Test { [[
var int a=0;
]],
    _ana = {
        reachs = 1,
        isForever = true,
    },
}

Test { [[
var int a;
a = do/_
    var int b=0;
end;
]],
    _ana = {
        reachs = 1,
        unreachs = 1,
        isForever = true,
    },
}

Test { [[
var int a=0;
par/or do
    a = 1;
with
    a = 2;
end;
escape a;
]],
    _ana = {
        acc = 1,
        abrt = 1,
    },
}

-- BIG // FULL // COMPLETE
Test { [[
input int KEY;
if true then escape 50; end
par do
    var int pct=0, dt=0, step=0, ship=0, points=0;
    var int win = 0;
    loop do
        if win then
            // next phase (faster, harder, keep points)
            step = 0;
            ship = 0;
            if dt > 100 then
                dt = dt - 50;
            end
            if pct > 10 then
                pct = pct - 1;
            end
        else
            // restart
            pct    = 35;    // map generator (10 out of 35 for a '#')
            dt     = 500;   // game speed (500ms/step)
            step   = 0;     // current step
            ship   = 0;     // ship position (0=up, 1=down)
            points = 0;     // number of steps alive
        end
        await KEY;
        win = do/_ par do
                loop do
                    await (dt)ms;
                    step = step + 1;

                    if step == 1 then
                        escape 1;           // finish line
                    end
                    points = points + 1;
                end
            with
                loop do
                    var int key = await KEY;
                    if key == 1 then
                        ship = 0;
                    end
                    if key == 1 then
                        ship = 1;
                    end
                end
end
            end;
        par/or do
            await 1s;
            await KEY;
        with
            if not win then
                loop do
                    await 100ms;
                    await 100ms;
                end
            end
        end
    end
with
    var int key = 1;
    loop do
        var int read1 = 1;
            read1 = 1;
        await 50ms;
        var int read2 = 1;
            read2 = 1;
        if read1==read2 and key!=read1 then
            key = read1;
            if key != 1 then
                async (read1) do
                    emit KEY => read1;
                end
            end
        end
    end
end
]],
    run = 50,
}

-->>> PAUSE

Test { [[
event bool a;
pause/if a do
end
escape 1;
]],
    run = 1,
}

Test { [[
input void A;
pause/if A do
end
escape 0;
]],
    --adj = 'line 2 : invalid expression',
    parser = 'line 2 : after `pause/if´ : expected expression',
    --parser = 'line 2 : after `A´ : expected `(´',
}

Test { [[
event void a;
var int v = await a;
escape 0;
]],
    --env = 'line 2 : event type must be numeric',
    --env = 'line 2 : invalid attribution',
    env = 'line 2 : arity mismatch',
    --env = 'line 2 : invalid attribution (int vs void)',
}

Test { [[
event void a;
pause/if a do
end
escape 0;
]],
    --env = 'line 2 : event type must be numeric',
    env = 'line 2 : arity mismatch',
    --env = 'line 2 : invalid attribution',
    --env = 'line 2 : invalid attribution (bool vs void)',
}

Test { [[
input int A, B;
event bool a;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end
with
    pause/if a do
        var int v = await B;
        escape v;
    end
end
]],
    _ana = {
        unreachs = 1,
    },
    run = {
        ['1~>B'] = 1,
        ['0~>A ; 1~>B'] = 1,
        ['1~>A ; 1~>B ; 0~>A ; 3~>B'] = 3,
        ['1~>A ; 1~>A ; 1~>B ; 0~>A ; 3~>B'] = 3,
        ['1~>A ; 1~>B ; 1~>B ; 0~>A ; 3~>B'] = 3,
        ['1~>A ; 1~>B ; 0~>A ; 1~>A ; 2~>B ; 0~>A ; 3~>B'] = 3,
        ['1~>A ; 1~>B ; 0~>A ; 1~>A ; 0~>A ; 3~>B'] = 3,
        ['1~>A ; 1~>B ; 1~>A ; 2~>B ; 0~>A ; 3~>B'] = 3,
    },
}

-- TODO: nesting with same event
Test { [[
input int A,B;
event int a;
var int ret = 0;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end
with

    pause/if a do
        pause/if a do
            ret = await B;
        end
    end
end
escape ret;
]],
    run = {
        ['1~>B;1~>B'] = 1,
        ['0~>A ; 1~>B'] = 1,
        ['1~>A ; 1~>B ; 0~>A ; 0~>A ; 3~>B'] = 3,
        ['1~>A ; 1~>B ; 0~>A ; 1~>A ; 2~>B ; 0~>A ; 0~>A ; 3~>B'] = 3,
        ['1~>A ; 1~>B ; 0~>A ; 1~>A ; 0~>A ; 0~>A ; 3~>B'] = 3,
        ['1~>A ; 1~>B ; 1~>A ; 2~>B ; 0~>A ; 0~>A ; 3~>B'] = 3,
    },
}

Test { [[
input int A, B, Z;
event bool a, b;
var int ret = 0;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end
with
    loop do
        var int v = await B;
        emit b => v;
    end
with
    pause/if a do
        pause/if b do
            ret = await Z;
        end
    end
end
escape ret;
]],
    run = {
        ['1~>Z'] = 1,
        ['1~>A ; 10~>Z ; 1~>B ; 10~>Z ; 0~>B ; 10~>Z ; 0~>A ; 5~>Z'] = 5,
        ['1~>A ; 1~>B ; 0~>B ; 10~>Z ; 0~>A ; 1~>B ; 5~>Z ; 0~>B ; 100~>Z'] = 100,
    },
}

Test { [[
input int  A;
input int  B;
input void Z;
event int a;
var int ret = 0;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end
with
    pause/if a do
        await Z;
        ret = await B;
    end
end
escape ret;
]],
    run = {
        ['~>Z ; 1~>B'] = 1,
        ['0~>A ; 1~>B ; ~>Z ; 2~>B'] = 2,
        ['~>Z ; 1~>A ; 1~>B ; 0~>A ; 3~>B'] = 3,
        ['~>Z ; 1~>A ; 1~>B ; 1~>A ; 2~>B ; 0~>A ; 3~>B'] = 3,
    },
}

Test { [[
input int  A;
input void Z;
event bool a;
var int ret = 0;
par/or do
    emit a => 1;
    await A;
with
    pause/if a do
        do finalize with
            ret = 10;
    end
        await Z;
    end
end
escape ret;
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['1~>A'] = 10,
    },
}

Test { [[
input int  A;
input void Z;
event int a;
var int ret = 0;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end
with
    pause/if a do
        await 9s;
    end
    ret = 9;
with
    await 10s;
    ret = 10;
end
escape ret;
]],
    _ana = {
        acc = 1,     -- TODO: 0
    },
    run = {
        ['1~>A ; ~>5s ; 0~>A ; ~>5s'] = 10,
    },
}

Test { [[
input int  A,B,C;
event bool a;
var int ret = 50;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end
with
    pause/if a do
        ret = await B;
    end
with
    await C;
end
escape ret;
]],
    run = {
        ['1~>A ; 10~>B ; 1~>C'] = 50,
    },
}

Test { [[
input void C;
par/or do
    await C;
with
    await 1us;
end
var int v = await 1us;
escape v;
]],
    run = { ['~>1us; ~>C; ~>4us; ~>C']=3 }
}

Test { [[
input int  A;
event int a;
var int ret = 0;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end
with
    pause/if a do
        ret = await 9us;
    end
end
escape ret;
]],
    run = {
        ['~>1us;0~>A;~>1us;0~>A;~>19us'] = 12,
        --['~>1us;1~>A;~>1s;0~>A;~>19us'] = 11,
        --['~>1us;1~>A;~>5us;0~>A;~>5us;1~>A;~>5us;0~>A;~>9us'] = 6,
    },
}

Test { [[
event bool in_tm;
pause/if in_tm do
    async do
        loop i in [0 |> 5[ do
        end
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
event bool e;

par/or do
    par/and do
        pause/if e do
            await 1s;
        end
    with
        emit e => true;
    end
    escape -1;
with
    await 2s;
    escape 1;
end
]],
    _ana = {acc=true},
    run = {['~>2s']=1},
}

Test { [[
event bool e;

par/or do
    par/and do
        pause/if e do
            await 1s;
        end
    with
        emit e => true;
        emit e => false;
    end
    escape -1;
with
    await 2s;
    escape 1;
end
]],
    _ana = {acc=true},
    run = {['~>2s']=-1},
}

--<<< PAUSE

-- TIGHT LOOPS

Test { [[
loop i in [0 |> 10[ do
    i = 0;
end
]],
    env = 'line 2 : read-only variable',
}

Test { [[
loop do end
]],
    tight = 'line 1 : tight loop',
}
Test { [[
loop i do end
]],
    tight = 'line 1 : tight loop',
}
Test { [[
loop i in [0 |> 10[ do end
escape 2;
]],
    run = 2,
}
Test { [[
var int v=1;
loop i in [0|>v[ do end
]],
    tight = 'line 2 : tight loop',
}

-- INFINITE LOOP/EXECUTION
Test { [[
event void e, f;
par do
    loop do
        par/or do
            emit f;         // 18
        with
            await f;        // 20
        end
        await e;            // 23
    end
with
    loop do
        par/or do
            await f;        // 8
        with
            emit e;         // 11
            await FOREVER;
        end
    end
end
]],
    _ana = {
        isForever = true,
        acc = 3,
    },
    awaits = 0,
    run = 0,
}

Test { [[
event void e, f;
par do
    loop do
        par/or do
            await f;        // 8
        with
            emit e;         // 11
            await FOREVER;
        end
    end
with
    loop do
        par/or do
            emit f;         // 18
        with
            await f;        // 20
        end
        await e;            // 23
    end
end
]],
    _ana = {
        isForever = true,
        acc = 3,
    },
    awaits = 0,
    run = 0,
}

Test { [[
event void e, f;
par do
    loop do
        par/or do
            emit e;     // 8
            await FOREVER;
        with
            await f;
        end
    end
with
    loop do
        await e;        // 17
        par/or do
            emit f;     // 20
        with
            await f;    // 22
        end
    end
end
]],
    _ana = {
        isForever = true,
        acc = 2,
    },
    awaits = 0,
    run = 0
}

Test { [[
event void e, k1, k2;
par do
    loop do
        par/or do
            emit e;
            await FOREVER;
        with
            await k1;
        end
        emit k2;
    end
with
    loop do
        await e;
        par/or do
            emit k1;
        with
            await k2;
        end
    end
end
]],
    _ana = {
        isForever = true,
        acc = 1,
    },
    awaits = 1,
    run = 0,
}
Test { [[
event void e, f;
par do
    loop do
        par/or do
            await f;        // 8
        with
            emit e;         // 11
            await FOREVER;
        end
    end
with
    loop do
        par/or do
            await f;        // 20
        with
            emit f;         // 18
        end
        await e;            // 23
    end
end
]],
    _ana = {
        isForever = true,
        acc = 3,
    },
    awaits = 0,
    run = 0,
}

Test { [[
event void e, f;
par do
    loop do
        par/or do
            await f;        // 20
        with
            emit f;         // 18
        end
        await e;            // 23
    end
with
    loop do
        par/or do
            await f;        // 8
        with
            emit e;         // 11
            await FOREVER;
        end
    end
end
]],
    _ana = {
        isForever = true,
        acc = 3,
    },
    awaits = 0,
    run = 0,
}

Test { [[
event void e, f;
par do
    loop do
        await e;        // 17
        par/or do
            await f;    // 22
        with
            emit f;     // 20
        end
    end
with
    loop do
        par/or do
            await f;
        with
            emit e;     // 8
            await FOREVER;
        end
    end
end
]],
    _ana = {
        isForever = true,
        acc = 2,
    },
    awaits = 0,
    run = 0
}

Test { [[
event void e, k1, k2;
par do
    loop do
        await e;
        par/or do
            await k2;
        with
            emit k1;
            await FOREVER;
        end
    end
with
    loop do
        par/or do
            await k1;
        with
            emit e;
            await FOREVER;
        end
        emit k2;
    end
end
]],
    _ana = {
        isForever = true,
        acc = 1,
    },
    awaits = 1,
    run = 0,
}
Test { [[
event void e;
loop do
    par/or do
        await e;
    with
        emit e;
        await FOREVER;
    end
end
]],
    _ana = {
        isForever = true,
        acc = true,
    },
    awaits = 1,
    run = 0,
}
Test { [[
event void e;
loop do
    par/or do
        await e;
        await e;
    with
        emit e;
        emit e;
        await FOREVER;
    end
end
]],
    _ana = {
        isForever = true,
        acc = true,
    },
    awaits = 1,
    run = 0,
}
Test { [[
event void e;
loop do
    watching e do
        emit e;
        await FOREVER;
    end
end
]],
    _ana = {
        isForever = true,
        acc = true,
    },
    awaits = 1,
    run = 0,
}
Test { [[
var int ret = 0;
event void e;
par/or do
    loop do
        await e;
        ret = ret + 1;
    end
with
    every e do
        ret = ret + 1;
    end
with
    emit e;
    emit e;
    emit e;
end
escape ret;
]],
    _ana = { acc=true },
    run = 3;
}

-->>> DONT CARE, NONE

Test { [[
var int a = _;
loop _ in [0|>10[ do
end

do/_
    escape;
end

await 1ms/_;

escape 1;
]],
    run = 1,
}

--<<< DONT CARE, NONE

-->>> REENTRANT

if REENTRANT then

Test { [[
input int E,C;

par do
    async do
        emit E => 10;
    end
    await FOREVER;
with
    var int ret = 0;
    par/and do
        var int v = await E;
        var int x = 1000;
        do _ceu_sys_go(__ceu_app, _CEU_IN_F, &&x);
            finalize with nothing; end;
        ret = ret + v;
    with
        var int v = await E;
        ret = ret + v;
    end
    escape ret;
end
]],
    wrn = true,
    _ana = {acc=true},
    run = 20,
}

end

--<<< REENTRANT

-->> ASYNCS // THREADS
--while true do

Test { [[
var int  a=10, b=5;
var& int p = &b;
async/thread do
end
escape a + b + p;
]],
    run = 20,
}

Test { [[
var int ret =
    async/thread do
    end;
escape (ret == 1);
]],
    run = 1,
}

Test { [[
var int  a=10, b=5;
var& int p = &b;
async/thread (a, p) do
    a = a + p;
    atomic do
        p = a;
    end
end
escape a + b + p;
]],
    run = 45,
}

Test { [[
var int  a=10, b=5;
var& int p = &b;
var int ret =
    async/thread (a, p) do
        a = a + p;
        atomic do
            p = a;
        end
    end;
escape (ret==1) + a + b + p;
]],
    run = 46,
}

Test { [[
atomic do
    escape 1;
end
]],
    props = 'line 2 : not permitted inside `atomic´',
}

Test { [[
native do
    ##define ceu_out_isr_on();
    ##define ceu_out_isr_off();
end
async do
    atomic do
        nothing;
    end
end
escape 1;
]],
    --props = 'line 2 : not permitted outside `thread´',
    run = 1,
}

Test { [[
var int x = 0;
par/and do
    x = 1;
with
    var& int p = &x;
    p = 2;
    async/thread (p) do
        p = 2;
    end
end
escape x;
]],
    _ana = {
        acc = 4,
    },
    run = 2,
}

Test { [[
var int x = 0;
par/and do
    x = 1;
with
    var& int p = &x;
    p = 2;
    async/thread (p) do
        atomic do
            p = 2;
        end
    end
end
escape x;
]],
    _ana = {
        acc = 4,
    },
    run = 2,
}

Test { [[
var int  a=10, b=5;
var& int p = &b;
async/thread (a, p) do
    a = a + p;
    p = a;
end
escape a + b + p;
]],
    run = 45,
}

Test { [[
var int  a=10, b=5;
var int&& p = &&b;
async/thread (p) do
    *p = 1;
end
escape 1;
]],
    fin = 'line 3 : unsafe access to pointer "p" across `async/thread´',
}

Test { [[
native _usleep;
var int  a=10, b=5;
var& int p = &b;
par/and do
    async/thread (a, p) do
        _usleep(100);
        a = a + p;
        p = a;
    end
with
    p = 2;
end
escape a + b + p;
]],
    _ana = {
        acc = true,
    },
    run = 36,
}

Test { [[
var int  a=10, b=5;
var& int p = &b;
async/thread (a, p) do
    atomic do
        a = a + p;
        p = a;
    end
end
escape a + b + p;
]],
    run = 45,
}

for i=1, 50 do
    Test { [[
native do
    ##include <unistd.h>
end
var int ret = 1;
var& int p = &ret;
par/or do
    async/thread (p) do
        atomic do
            p = 2;
        end
    end
with
end
native _usleep;
_usleep(]]..i..[[);
escape ret;
]],
        usleep = true,
        run = 1,
    }
end

for i=1, 50 do
    Test { [[
native do
    ##include <unistd.h>
end
var int ret = 0;
var& int p = &ret;
par/or do
    async/thread (p) do
native _usleep;
        _usleep(]]..i..[[);
        atomic do
            p = 2;
        end
    end
with
    ret = 1;
end
_usleep(]]..i..[[+1);
escape ret;
]],
        complete = (i>1),   -- run i=1 for sure
        usleep = true,
        run = 1,
        _ana = { acc=1 },
    }
end

Test { [[
var int  v1=10, v2=5;
var& int p1 = &v1;
var& int p2 = &v2;

par/and do
    async/thread (v1, p1) do
        atomic do
            p1 = v1 + v1;
        end
    end
with
    async/thread (v2, p2) do
        atomic do
            p2 = v2 + v2;
        end
    end
end
escape v1+v2;
]],
    run = 30,
}

Test { [[
var int  v1=0, v2=0;
var& int p1 = &v1;
var& int p2 = &v2;

native _calc, _assert;
native do
    int calc ()
    {
        int ret, i, j;
        ret = 0;
        for (i=0; i<10; i++) {
            for (j=0; j<10; j++) {
                ret = ret + i + j;
            }
        }
        escape ret;
    }
end

par/and do
    async/thread (p1) do
        var int ret = _calc();
        atomic do
            p1 = ret;
        end
    end
with
    async/thread (p2) do
        var int ret = _calc();
        atomic do
            p2 = ret;
        end
    end
end
native do ##include <assert.h> end
_assert(v1 == v2);
escape v1;
]],
    run = 900,
}

Test { [[
native _assert;
var int  v1=0, v2=0;
var& int p1 = &v1;
var& int p2 = &v2;

par/and do
    async/thread (p1) do
        var int ret = 0;
        loop i in [0 |> 10[ do
            loop j in [0 |> 10[ do
                ret = ret + i + j;
            end
        end
        atomic do
            p1 = ret;
        end
    end
with
    async/thread (p2) do
        var int ret = 0;
        loop i in [0 |> 10[ do
            loop j in [0 |> 10[ do
                ret = ret + i + j;
            end
        end
        atomic do
            p2 = ret;
        end
    end
end
native do ##include <assert.h> end
_assert(v1 == v2);
escape v1;
]],
    run = 900,
}

Test { [[
var int  v1=0, v2=0;
var& int p1 = &v1;
var& int p2 = &v2;

native do
    int calc ()
    {
        int ret, i, j;
        ret = 0;
        for (i=0; i<50000; i++) {
            for (j=0; j<50000; j++) {
                ret = ret + i + j;
            }
        }
        escape ret;
    }
end

par/and do
    async/thread (p1) do
native _calc, _assert;
        var int ret = _calc();
        atomic do
            p1 = ret;
        end
    end
with
    async/thread (p2) do
        var int ret = _calc();
        atomic do
            p2 = ret;
        end
    end
end
native do ##include <assert.h> end
_assert(v1 == v2);
escape v1;
]],
    --run = false,
    run = 1066784512,
}

Test { [[
native _assert;
var int  v1=0, v2=0;
var& int p1 = &v1;
var& int p2 = &v2;

par/and do
    async/thread (p1) do
        var int ret = 0;
        loop i in [0 |> 50000[ do
            loop j in [0 |> 50000[ do
                ret = ret + i + j;
            end
        end
        atomic do
            p1 = ret;
        end
    end
with
    async/thread (p2) do
        var int ret = 0;
        loop i in [0 |> 50000[ do
            loop j in [0 |> 50000[ do
                ret = ret + i + j;
            end
        end
        atomic do
            p2 = ret;
        end
    end
end
native do ##include <assert.h> end
_assert(v1 == v2);
escape v1;
]],
    run = 1066784512,
    --run = false,
-- thr.c
--./a.out  17.41s user 0.00s system 180% cpu 9.629 total
-- me (isTmp=true)
--./a.out  16.80s user 0.02s system 176% cpu 9.525 total
-- me (isTmp=false)
--./a.out  30.36s user 0.04s system 173% cpu 17.476 total
}

Test { [[
pre native do
    ##include <unistd.h>
    int V = 0;
end
par/or do
    async do
        loop i in [0 |> 3[ do
native _usleep;
            _usleep(500);
        end
    end
with
    async/thread do
        loop i in [0 |> 2[ do
native _V;
            _V = _V + 1;
            _usleep(500);
        end
    end
end
escape _V;
]],
    _ana = {acc=1},
    usleep = true,
    run = 2,
}

-- THREADS / EMITS

Test { [[
input int A;
par/or do
    await A;
with
    async/thread do
        emit A=>10;
    end
end;
escape 10;
]],
    _ana = {
        isForever = false,
    },
    --run = 10,
    props = 'not permitted inside `thread´',
    --props = 'line 6 : invalid `emit´',
}
Test { [[
input int A;
par/or do
    await A;
with
    async do
        emit A=>10;
    end
end;
escape 10;
]],
    _ana = {
        isForever = false,
    },
    run = 10,
}

Test { [[
var int a=1;
var& int pa = &a;
async/thread (pa) do
    emit 1min;
    pa = 10;
end;
escape a + 1;
]],
    --run = 11,
    props = 'not permitted inside `thread´',
}
Test { [[
var int a=1;
var& int pa = &a;
async (pa) do
    emit 1min;
    pa = 10;
end;
escape a + 1;
]],
    run = 11,
}

Test { [[
par do
    var int v1=4,v2=4;
    par/or do
        await 10ms;
        v1 = 1;
    with
        await 10ms;
        v2 = 2;
    end
    escape v1 + v2;
with
    async/thread do
        emit 5ms;
        emit(5000)ms;
    end
end
]],
    _ana = {
        isForever = false,
        abrt = 3,
    },
    --run = 5,
    --run = 3,
    --todo = 'nd excpt',
    props = 'not permitted inside `thread´',
}
Test { [[
par do
    var int v1=4,v2=4;
    par/or do
        await 10ms;
        v1 = 1;
    with
        await 10ms;
        v2 = 2;
    end
    escape v1 + v2;
with
    async do
        emit 5ms;
        emit(5000)ms;
    end
end
]],
    _ana = {
        isForever = false,
        abrt = 3,
    },
    run = 5,
    --run = 3,
    --todo = 'nd excpt',
}

Test { [[
input int A;
par do
    async/thread do end
with
    await A;
    escape 1;
end
]],
    run = { ['1~>A']=1 },
}

Test { [[
native do ##include <assert.h> end
native _assert;
input void A;
var int ret = 0;
par/or do
    loop do
        var int late = await 10ms;
        ret = ret + late;
        _assert(late <= 10000);
    end
with
    loop do
        var int i = 0;
        par/or do
            var int t;
            t = await 1s;
        with
            loop do
                await A;
                i = i + 1;
            end
        end
    end
with
    async/thread do
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
    end
end
escape ret;
]],
    --run = 72000,
    props = 'not permitted inside `thread´',
}
Test { [[
native do ##include <assert.h> end
native _assert;
input void A;
var int ret = 0;
par/or do
    loop do
        var int late = await 10ms;
        ret = ret + late;
        _assert(late <= 10000);
    end
with
    loop do
        var int i = 0;
        par/or do
            var int t;
            t = await 1s;
            if t then end;
        with
            loop do
                await A;
                i = i + 1;
            end
        end
    end
with
    async do
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
        emit 12ms;
        emit A;
    end
end
escape ret;
]],
    run = 72000,
}

Test { [[
input int P2;
par do
    loop do
        par/or do
            var int p2 = await P2;
            if p2 == 1 then
                escape 0;
            end;
        with
            loop do
                await 200ms;
            end;
        end;
    end;
with
    async/thread do
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 1;
    end;
    await FOREVER;      // TODO: ele acha que o async termina
end;
]],
    --run = 0,
    props = 'not permitted inside `thread´',
}
Test { [[
input int P2;
par do
    loop do
        par/or do
            var int p2 = await P2;
            if p2 == 1 then
                escape 0;
            end;
        with
            loop do
                await 200ms;
            end;
        end;
    end;
with
    async do
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 0;
        emit P2 => 1;
    end;
    await FOREVER;      // TODO: ele acha que o async termina
end;
]],
    run = 0,
}

Test { [[
var int ret = 0;
input void A;
par/and do
    await 1s;
    ret = ret + 1;
with
    async do
        emit 1s;
    end
    ret = ret + 1;
with
    async/thread do
        atomic do
        end
    end
    ret = ret + 1;
with
    async do
        emit A;
    end
    ret = ret + 1;
end
escape ret;
]],
    run = { ['~>A;~>1s'] = 4 },
}

-- ASYNC/NONDET

Test { [[
native _int;
var _int[2] v = [];
var _int&& p = &&v[0];
par/and do
    v[0] = 1;
with
    p[1] = 2;
end
escape v[0] + v[1];
]],
    _ana = {
        acc = 1,
    },
    --fin = 'line 6 : pointer access across `await´',
    run = 3;
}
Test { [[
native _int;
var _int[2] v = [];
par/and do
    v[0] = 1;
with
    var _int&& p = &&v[0];
    p[1] = 2;
end
escape v[0] + v[1];
]],
    _ana = {
        acc = 1,
    },
    run = 3,
}
Test { [[
vector[2] int v = [0,0];
vector[2] int p = [0,0];
par/and do
    v[0] = 1;
with
    p[1] = 2;
end
escape v[0] + p[1];
]],
    run = 3,
}

Test { [[
var int x = 0;
async do
    x = 2;
end
escape x;
]],
    locs = 'line 3 : internal identifier "x" is not declared',
}

Test { [[
var int x = 0;
async/thread do
    x = 2;
end
escape x;
]],
    locs = 'line 3 : internal identifier "x" is not declared',
}

Test { [[
var int x = 0;
par/and do
    x = 1;
with
    async (x) do
        x = 2;
    end
end
escape x;
]],
    _ana = { acc=1 },
    run = 2,
}

Test { [[
var int x = 0;
par/and do
    x = 1;
with
    async/thread (x) do
        x = 2;
    end
end
escape x;
]],
    _ana = { acc=1 },
    run = 2,
}

Test { [[
var int x = 0;
par/and do
    x = 1;
with
    async/thread (x) do
        x = 2;
    end
end
escape x;
]],
    _ana = {
        acc = 1,
    },
    run = 2,
}

Test { [[
var int x = 0;
par/and do
    await 1s;
    x = 1;
with
    var int y = x;
    async/thread (y) do
        y = 2;
native _usleep;
        _usleep(50);
    end
    x = x + y;
end
escape x;
]],
    run = { ['~>1s']=3 },
    _ana = {
        acc = true,
    },
}

Test { [[
var int x = 0;
par/and do
    await 1s;
    x = 1;
with
    var int y = x;
    async/thread (y) do
        y = 2;
native _usleep;
        _usleep(50);
    end
    x = x + y;
end
escape x;
]],
    run = { ['~>1s']=3 },
    safety = 2,
    _ana = {
        acc = 3,
    },
}

Test { [[
var int x  = 0;
var int&& p = &&x;
par/and do
    *p = 1;
with
    var int y = x;
    async/thread (y) do
        y = 2;
    end
    x = x + y;
end
escape x;
]],
    _ana = {
        acc = 3,
    },
    run = 3,
}

Test { [[
native/plain _int;
var _int[10] x = [];
async/thread (x) do
    x[0] = 2;
end
escape x[0];
]],
    run = 2,
    --gcc = 'error: lvalue required as left operand of assignment',
}

Test { [[
vector[10] int x = [0];
async/thread (x) do
    x[0] = 2;
end
escape x[0];
]],
    run = 2,
    --gcc = 'error: lvalue required as left operand of assignment',
}

Test { [[
vector[10] int x = [0,1];
par/and do
    async/thread (x) do
native _usleep;
        _usleep(100);
        x[0] = x[1] + 2;
    end
with
    x[1] = 5;
end
escape x[0];
]],
    run = 7,
    _ana = {
        acc = 2,
    },
    --gcc = 'error: lvalue required as left operand of assignment',
}

Test { [[
var int v = 1;
async (v) do
    do finalize with
        v = 2;
    end
end;
escape v;
]],
    props = 'line 3 : not permitted inside `async´',
}
Test { [[
var int v = 1;
async/thread (v) do
    do finalize with
        v = 2;
    end
end;
escape v;
]],
    props = 'line 3 : not permitted inside `thread´',
}

Test { [[
native _f;
native do
    int f (int v) {
        escape v + 1;
    }
end
var int a = 0;
async/thread (a) do
    a = _f(10);
end
escape a;
]],
    run = 11,
}

Test { [[
var int ret = 0;
async (ret) do
    ret = do/_ escape 1; end;
end
escape ret;
]],
    run = 1,
}
Test { [[
var int ret = 0;
async/thread (ret) do
    ret = do/_ escape 1; end;
end
escape ret;
]],
    run = 1,
}

Test { [=[
    async/thread do
    end
    loop i in [0 |> 100[ do
        await 1s;
    end
    escape 1;
]=],
    run = {['~>100s;~>100s']=1},
}
--end
--do return end
--<<< THREADS / EMITS
--<<< ASYNCS / THREADS

-->>> LUA

Test { [==[
[[
    a = 1
]]
var int a = [[a]];
escape a;
]==],
    run = 1,
}

Test { [==[
[[
    --[[oi]]
    a = 1
]]
var int a = [[a]];
escape a;
]==],
    parser = 'line 3 : after `1´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `;´',
}

Test { [==[
[=[
    --[[oi]]
    a = 1
]=]
var int a = [[a]];
escape a;
]==],
    run = 1,
}

Test { [=[
var bool v = [["ok" == 'ok']];
escape v;
]=],
    run = 1,
}

Test { [=[
var bool v = [[true]];
escape v;
]=],
    run = 1,
}

Test { [=[
var bool v = [[false]];
escape v;
]=],
    run = 0,
}

Test { [==[
[[
    print '*** END: 10 0'
]]
var int v = [[1]];
escape v;
]==],
    run = 10,
}

Test { [==[
[[
    aa $ aa
]]
escape 1;
]==],
    run = '1] lua error : [string "..."]:2: syntax error near \'$\'',
    --run = '2: \'=\' expected near \'$\'',
}

Test { [=[
var int a = [[1]];
[[
    a = @a+1
]]
var int ret = [[a]];
escape ret;
]=],
    run = 2,
}

Test { [=[
var int a = [[1]];
var int b = 10;
[[
    a = @a+@b
]]
var int ret = [[a]];
escape ret;
]=],
    run = 11,
}

Test { [=[

[[ error'oi' ]];
escape 1;
]=],
    run = '2] lua error : [string " error\'oi\' "]:1: oi',
}

Test { [=[
var int ret = [[ true ]];
escape ret;
]=],
    run = '1] lua error : number expected',
}
Test { [=[
var bool ret = [[ nil ]];
escape ret==false;
]=],
    run = 1,
}
Test { [=[

var int ret = [[ nil ]];
escape ret;
]=],
    run = '2] lua error : number expected',
}

Test { [=[
native _char;
native/nohold _strcmp;
var byte&& str = "oioioi";
[[ str = @str ]]
var bool ret = [[ str == 'oioioi' ]];
vector[10] byte cpy = [[ str ]];
escape ret and (not _strcmp(str,&&cpy as _char&&));
]=],
    run = 1,
}

Test { [=[
native/nohold _strcmp, _strcpy;
vector[10] byte str;
_strcpy(&&str,"oioioi");
[[ str = @str ]]
var bool ret = [[ str == 'oioioi' ]];

vector[10] byte cpy;
var byte&& ptr = cpy;
ptr = [[ str ]];
escape ret and (not _strcmp(&&str,&&cpy));
]=],
    env = 'line 7 : types mismatch (`byte&&´ <= `byte[]´)',
}

Test { [=[
native/nohold _strcmp;
vector[10] byte str = [] .. "oioioi";
[[ str = @str ]]
var bool ret = [[ str == 'oioioi' ]];
vector[10] byte cpy;
vector&[10] byte ptr = &cpy;
ptr = [[ str ]];
native _char;
escape ret and (not _strcmp(&&str as _char&&,&&cpy as _char&&));
]=],
    run = 1,
}

Test { [=[
native/nohold _strcmp;
[[ str = '1234567890' ]]
vector[2] byte cpy = [[ str ]];
native _char;
escape (_strcmp(&&cpy as _char&&,"1") == 0);
]=],
    run = '3] runtime error: access out of bounds',
}

Test { [=[
native/nohold _strcmp;
[[ str = '1234567890' ]]
vector[2] byte cpy;
vector[20] byte cpy_;
vector&[] byte ptr = &cpy;
ptr = [[ str ]];
native _char;
escape (not _strcmp(&&cpy as _char&&,"1234567890"));
]=],
    run = '6] runtime error: access out of bounds',
}

Test { [=[
var int a = [[1]];
var int b = 10;
[[
    @a = @a+@b
    a = @a
]]
var int ret = [[a]];
escape ret;
]=],
    todo = 'error: assign to @a',
    run = 11,
}

Test { [=[
[[ ]] [[ ]] [[ ]]
escape 1;
]=],
    run = 1,
}
Test { [=[
[[ ]]
[[ ]]
[[ ]]
escape 1;
]=],
    run = 1,
}
Test { [=[
native/nohold _strcmp;

[[
-- this is lua code
v_from_lua = 100
]]

var int v_from_ceu = [[v_from_lua]];

[[
str_from_lua = 'string from lua'
]]
vector[100] byte str_from_ceu = [[str_from_lua]];
native _assert;
native _char;
_assert(0==_strcmp(&&str_from_ceu as _char&&, "string from lua"));

[[
print(@v_from_ceu)
v_from_lua = v_from_lua + @v_from_ceu
]]

//v_from_ceu = [[nil]];

var int ret = [[v_from_lua]];
escape ret;
]=],
    run = 200,
}

Test { [=[
var int a=0;
var void&& ptr1 = &&a;
[[ ptr = @ptr1 ]];
var void&& ptr2 = [[ ptr ]];
escape ptr2==&&a;
]=],
    run = 1,
}

Test { [=[
var bool b1 = true;
var bool b2 = false;
var int ret = [[ @b1==true and @b2==false ]];
[[
    b1 = @b1
    b2 = @b2
]];
var bool b1_ = [[b1]];
var bool b2_ = [[b2]];
escape ret + b1_ + b2_;
]=],
    run = 2,
}

Test { [=[

[[
    (0)();
]];
escape 1;
]=],
    run = '2: attempt to call a number value',
}

Test { [=[

var int ret = [[
    (0)();
]];
escape ret;
]=],
    --run = 1,
    run = '2: attempt to call a number value',
}

Test { [=[
do
    var f32 f = 0;
    [[assert(math.type(@f)=='float')]];
end
do
    var f64 f = 0;
    [[assert(math.type(@f)=='float')]];
end

var int   i = 0;
var float f = 0;
var bool is_int   = [[math.type(@i)=='integer']];
var bool is_float = [[math.type(@f)=='float']];

[[assert(math.type(@(1.1))=='float')]];
[[assert(math.type(@(1.0))=='float')]];
[[assert(math.type(@(1))=='integer')]];

escape is_int+is_float;
]=],
    run = 2,
}

Test { [=[
code/instantaneous Fx (void)=>int do
    var int v = [[ 1 ]];
    escape v;
end
escape Fx();
]=],
    run = 1,
}

Test { [=[
var float v1 = [[ 0.5 ]];
var float v2 = 0.5;
escape v1==v2;
]=],
    run = 1,
}

--<<< LUA

-->>> CLASSES, ORGS, ORGANISMS
--do return end

Test { [[
code/delayed F (void)=>void
do
    escape 1;
end
escape 1;
]],
    parser = 'line 1 : after `code/delayed´ : expected `/recursive´ or abstraction identifier',
}

Test { [[
code/delayed Fx (void)=>void
do
    escape 1;
end
escape 1;
]],
    wrn = true,
    --adj = 'line 3 : invalid `escape´',
    run = 1,
}

Test { [[
code/delayed Tx (var int x)=>void
do
end
escape 1;
]],
    wrn = true,
    gcc = '1:9: error: unused variable ‘__ceu_x_1’ [-Werror=unused-variable]',
}
Test { [[
code/delayed Tx (var int x)=>void
do
    if x then end;
end
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { [[
code/delayed Tx (var int x)=>void
do
    var int v;
end

native do
    int V = sizeof(CEU_T);
end

native _V;
var Tx t;
escape _V;
]],
    todo = 'recalculate',
    run = 8,    -- 2/2 (trl0) 0 (x) 4 (y)
}

Test { [[
code/delayed Tx (void)=>void do end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/delayed Tx (var int x)=>void
do
    var int v;
end

native do
    int V = sizeof(CEU_T);
end
native _V;
escape _V;
]],
    todo = 'recalculate',
    run = 8,
}

Test { [[
code/delayed Tx (var int a)=>void
do
end

native/plain _TCEU_T;
var _TCEU_T t = _TCEU_T();
t.a = 1;
escape t.a;
]],
    wrn = true,
    gcc = 'error: unknown type name ‘TCEU_T’',
    --run = 1,
}

Test { [[
call TestX(5);
escape 0;
]],
    tops = 'line 1 : abstraction "TestX" is not declared',
}

Test { [[
code/delayed Tx (void)=>void do end
Tx();
escape 1;
]],
    run = 1,
}

Test { [[
code/delayed Tx (void)=>void do end
par/or do
    Tx();
with
    Tx();
end

input void OS_START;
await OS_START;

escape 1;
]],
    run = 1,
}


Test { [[
native _SDL_MouseButtonEvent;
input _SDL_MouseButtonEvent&& SDL_MOUSEBUTTONUP;
code/delayed Tx (void)=>void do
    var _SDL_MouseButtonEvent&& but = await SDL_MOUSEBUTTONUP;
end
await FOREVER;
]],
    --run = 1,
    _ana = {
        isForever = true,
    },
}

Test { [[
code/delayed Tx (void)=>void do
    await FOREVER;
end

var Tx t;

await 1s;

escape 1;
]],
    run = { ['~>1s'] = 1 },
}

Test { [[
native do
    int V = 1;
end

code/delayed Xx (void)=>void do
    every 1s do
native _V;
        _V = _V + 1;
    end
end

event bool pse;
par/or do
    pause/if pse do
        par do
            await Xx();
        with
            await Xx();
        end
    end
with
    emit pse=>true;
    await 5s;
end

escape _V;
]],
    _ana = {acc=true},
    run = {['~>5s']=1},
}

Test { [[
code/delayed Code (var int x) => int
do
    x = x + 1;
    await 1s;
    x = x + 1;
    escape x;
end
var int a = await Code(1);
escape a;
]],
    run = { ['~>1s']=3 },
}

Test { [[
every Code(1) do
end
]],
    parser = 'line 1 : after `every´ : expected internal identifier or `(´',
}
Test { [[
code/delayed Code (void)=>void;
await Code(1) until true;
]],
    env = 'TODO: until not allowed',
}
Test { [[
await 1s until true;
]],
    env = 'TODO: until not allowed',
}
Test { [[
code/delayed Code (var int x) => int
do
    var int xx = x + 1;
    await 1s;
    escape xx+1;
end
var int a = await Code(1);
escape a;
]],
    run = { ['~>1s']=3 },
}
Test { [[
native _X;
native do
    int X = 0;
end
code/delayed Code (var int x) => int
do
    var int xx = x + 1;
    await 1s;
    _X = xx + 1;
    escape xx+1;
end
await Code(1);
escape _X;
]],
    run = { ['~>1s']=3 },
}

Test { [[
code/delayed Code (var int x) => int
do
    x = x + 1;
    await 1s;
    escape x;
end
var int a =
    watching Code(10) do
        escape 1;
    end;

escape a;
]],
    _ana = {acc=1},
    run = 1,
}

Test { [[
code/delayed Code (var int x) => int
do
    x = x + 1;
    await 1s;
    escape x;
end
var int a =
    watching Code(10) do
        await 5s;
        escape 1;
    end;

escape a;
]],
    run = {['~>1s']=11 },
}

Test { [[
data Data with
    var int v;
end

code/delayed Code (var& Data d, var  int ini) => int
do
    d.v = ini;
    every 1s do
        d.v = d.v + 1;
    end
end

var Data d = Data(0);

var int a =
    watching Code(&d, 10) do
        var int ret = 0;
        watching 5s do
            every 1s do
                ret = ret + d.v;
            end
        end
        escape ret;
    end;

escape a;
]],
    run = {['~>10s']=50 },
}

Test { [[
data Data with
    var& int v;
end

code/delayed Code (var& Data d, var  int ini) => int
do
    var int v = ini;
    d.v = &v;
    every 1s do
        v = v + 1;
    end
end

var Data d = Data(0);

var int a =
    watching Code(&d, 10) do
        var int ret = 0;
        watching 5s do
            every 1s do
                ret = ret + d.v;
            end
        end
        escape ret;
    end;

escape a;
]],
    run = {['~>10s']=50 },
}

-- REFS: void&
Test { [[
var int v = 10;
var& void p = &v;
escape *(&&p as int&&);
]],
    run = 10,
}
Test { [[
code/delayed Tx (var& void p)=>int do
    escape *(&&p as int&&);
end

var int v = 10;
var int ret = await Tx(&v);
escape ret;
]],
    run = 10,
}

Test { [[
code/delayed Tx (void)=>void do
end
event Tx a;
escape 0;
]],
    env = 'line 4 : invalid event type',
}

Test { [[
code/delayed Tx (void)=>void do end
var Tx a = 1;
escape 0;
]],
    env = 'line 4 : types mismatch',
}

Test { [[
code/delayed Tx (void)=>void do
    await Tx();
end
escape 0;
]],
    tops = 'line 2 : abstraction "Tx" is not declared',
}

Test { [[
code/delayed Tx (var& int a)=>void do
    a = 5;
end
var int a = 0;
spawn Tx(&a);
escape a;
]],
    run = 5,
}

Test { [[
native do
    int V = 10;
end
code/delayed Tx (void)=>void do
native _V;
    _V = 100;
end
spawn Tx();
escape _V;
]],
    run = 100,
}

Test { [[
code/delayed Tx (var& int a)=>void do
    a = do/_
            escape 5;
        end;
end
var int a = 0;
spawn Tx(&a);
escape a;
]],
    run = 5,
}

Test { [[
input void OS_START;

code/delayed Tx (var& int a)=>void do
    await FOREVER;
end

var int v = 0;
watching Tx(&v) do
    v = 5;
    await OS_START;
end
escape v;
]],
    run = 5,
}

Test { [[
native _V;
native do
    int V = 1;
end

code/delayed Jj (void)=>void do
    _V = _V * 2;
end

code/delayed Tx (void)=>void do
    spawn Jj();
    _V = _V + 1;
end

spawn Tx();
_V = _V*3;
spawn Tx();
_V = _V*3;
spawn Tx();
_V = _V*3;
escape _V;
]],
    run = 345;
}

Test { [[
native _V;
native do
    int V = 1;
end

code/delayed Jj (void)=>void do
    _V = _V * 2;
end

code/delayed Tx (void)=>void do
    spawn Jj();
    _V = _V + 1;
end

input void OS_START;

spawn Tx();
_V = _V*3;
spawn Tx();
_V = _V*3;
spawn Tx();
_V = _V*3;

await OS_START;
escape _V;
]],
    run = 345;
}

Test { [[
native _V;
native do
    int V = 1;
end;

code/delayed Tx (void)=>void do
    event void e;
    emit e;
    _V = 10;
end

do
    spawn Tx();
end
escape _V;
]],
    run = 10,
}

Test { [[
class J with
do
end

class Tx with
do
    var J j;
    await FOREVER;
end

input void OS_START;
event void a;

var Tx t1;
var Tx t2;
emit a;
await OS_START;
escape 1;
]],
    run = 1;
}

Test { [[
class Tx with
    var int x=0;
do
    this.x = await 999ms;
end
var Tx t;
await 1s;
escape t.x;
]],
    run = {['~>1s']=1000},
}

Test { [[
native do
    int V = 10;
end

class Tx with
    event void e;
do
    await 1s;
    emit e;
native _V;
    _V = 1;
end

do
    var Tx t;
    await t.e;
end
await 1s;
escape _V;
]],
    run = { ['~>2s']=10 },
}
Test { [[
class Tx with
do
    native do
        int XXX = sizeof(CEU_T);
    end
end
escape _XXX > 0;
]],
    gcc = 'error: ‘CEU_T’ undeclared here (not in a function)',
}

Test { [[
class U with do end;
class Tx with
do
    native do
        int XXX = sizeof(CEU_U);
    end
end
escape _XXX > 0;
]],
    run = 1,
}

Test { [[
native _V;
native do
    int V = 1;
end

class J with
do
    _V = _V * 2;
end

class Tx with
do
    var J j;
    _V = _V + 1;
    await FOREVER;
end

input void OS_START;

var Tx t1;
_V = _V*3;
var Tx t2;
_V = _V*3;
var Tx t3;
_V = _V*3;
await OS_START;
escape _V;
]],
    run = 345;
}
Test { [[
var int a=8;
do
    var int a = 1;
    this.a = this.a + a + 5;
end
escape a;
]],
    wrn = true,
    --env = 'line 4 : invalid access',
    run = 14,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 8;
    var int a = 1;
    this.a = this.a + a + 5;
end
var Tx t;
input void OS_START;
await OS_START;
escape t.a;
]],
    gcc = 'error: duplicate member ‘a’',
    wrn = true,
    --run = 14,
    run = 8,
    --env = 'line 5 : cannot hide at top-level block',
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 8;
    do
        var int a = 1;
        this.a = this.a + a + 5;
    end
end
var Tx t;
input void OS_START;
await OS_START;
escape t.a;
]],
    wrn = true,
    run = 14,
}

Test { [[
class T2 with
do
end
class Tx with
    var T2 x;
do
end
var Tx a;
escape 1;
]],
    props = 'line 5 : not permitted inside an interface',
}
Test { [[
class T2 with
do
end
class Tx with
    var T2&&? x;
do
    var T2 xx;
    this.x = &&xx;
end
var Tx a;
escape 1;
]],
    run = 1,
}

Test { [[
class Test with
do
end

var Test&&? a = null; // leads to segfault

escape 1;
]],
    run = 1,
}

Test { [[
class T3 with
    var int v3=0;
do
end
class T2 with
    var T3 t3;
    var int v=0;
do
end
class Tx with
    var int v=0,v2=0;
    var T2 x;
do
end
var Tx a;
a.v = 5;
a.x.v = 5;
a.v2 = 10;
a.x.t3.v3 = 15;
escape a . v + a.x .v + a .v2 + a.x  .  t3 . v3;
]],
    props = 'line 6 : not permitted inside an interface',
}
Test { [[
class T3 with
    var int v3=0;
do
    await FOREVER;
end
class T2 with
    var T3&&? t3;
    var int v=0;
do
    var T3 t33;
    this.t3 = &&t33;
    await FOREVER;
end
class Tx with
    var int v=0,v2=0;
    var T2&&? x;
do
    var T2 xx;
    x = &&xx;
    await FOREVER;
end
var Tx a;
a.v = 5;
a.x!:v = 5;
a.v2 = 10;
a.x!:t3!:v3 = 15;
escape a . v + a.x! :v + a .v2 + a.x!  :  t3! : v3;
]],
    run = 35,
}

Test { [[
var int v;
class Tx with
    var int v;
    v = 5;
do
end
]],
    parser = 'line 3 : after `;´ : expected `var´ or `vector´ or `pool´ or `event´ or `code/instantaneous´ or `code/delayed´ or `interface´ or `input/output´ or `output/input´ or `input´ or `output´ or `do´',
}

Test { [[
var int v=0;
class Tx with
    var int v=5;
do
end
var Tx t;
escape t.v;
]],
    run = 5,
}
Test { [[
//var int v;
class Tx with
    var int v;
do
end
var Tx t;
escape t.v;
]],
    ref = 'line 6 : missing initialization for field "v" (declared in tests.lua:3)',
}
Test { [[
//var int v;
class Tx with
    var int v;
do
end
var Tx t with
    this.v = 1;
end;
var Tx x;
escape t.v + x.v;
]],
    ref = 'line 9 : missing initialization for field "v" (declared in tests.lua:3)',
}

Test { [[
var int v = 0;
class Tx with
    var int v=5;
do
end
var Tx t with
    this.v = 10;
end;
escape t.v;
]],
    run = 10,
}

Test { [[
class Foo with
  event void bar;
do
  // nothing
end

class Baz with
  var Foo&& qux;
do
  await (*qux).bar;
end

escape 1;
]],
    run = 1,
}

Test { [[
var int v = 0;
class Tx with
    var int v=5;
do
    this.v = 100;
end
var Tx t with
    this.v = 10;
end;
escape t.v;
]],
    run = 100,
}

Test { [[

var int v = 0;
class U with
    var int x = 10;
do
end

class Tx with
    var int v=5;
    var U u with
        this.x = 20;
    end;
do
    this.v = 100;
end
var Tx t with
    this.v = 10;
end;
escape t.v + t.u.x;
]],
    props = 'line 10 : not permitted inside an interface',
}

Test { [[
var int v = 0;
class U with
    var int x = 10;
do
end

class Tx with
    var int v=5;
    var U&& u;
do
    var U uu with
        this.x = 20;
    end;
    this.u = &&uu;
    this.v = 100;
end
var Tx t with
    this.v = 10;
end;
escape t.v + t.u:x;
]],
    ref = 'line 18 : missing initialization for field "u" (declared in tests.lua:9)',
}
Test { [[
class U with
    var int x = 10;
do
    await FOREVER;
end
var U&&? u;
var U uu with
    this.x = 20;
end;
u = &&uu;
escape u!:x;
]],
    run = 20,
}
Test { [[
class U with
    var int x = 10;
do
    await FOREVER;
end
var U&&? u;
var U uu with
    this.x = 20;
end;
u = &&uu;
escape u!:x;
]],
    run = 20,
}
Test { [[
class U with
    var int x = 10;
do
end
var U&&? u;
var U uu with
    this.x = 20;
end;
u = &&uu;
escape not u?;
]],
    run = 1,
}
Test { [[
var int v = 0;
class U with
    var int x = 10;
do
    await FOREVER;
end

class Tx with
    var int v=5;
    var U&&? u;
do
    var U uu with
        this.x = 20;
    end;
    this.u = &&uu;
    this.v = 100;
    await FOREVER;
end
var Tx t with
    this.v = 10;
end;
escape t.v + t.u!:x;
]],
    run = 120,
}

Test { [[
class Tx with
do
end

var Tx   t;
var Tx&&  p  = &&t;
var Tx&& && pp = &&p;

escape (p==&&t and pp==&&p and *pp==&&t);
]],
    run = 1,
}

Test { [[
var int&& v;
do
    var int i = 1;
    v = &&i;
end
escape *v;
]],
    --fin = 'line 4 : attribution requires `finalize´',
    --fin = 'line 4 : attribution to pointer with greater scope',
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:2)',
}
Test { [[
var& int v;
do
    var int i = 1;
    v = &i;
end
escape v;
]],
    --ref = 'line 4 : attribution to reference with greater scope',
    ref = 'line 1 : uninitialized variable "v" crossing compound statement (tests.lua:2)',
    --run = 1,
}

Test { [[
var int i = 0;
class Tx with
    var& int i;
do
    i = 10;
end
var Tx t;
escape i;
]],
    ref = 'line 7 : missing initialization for field "i" (declared in tests.lua:3)',
    --ref = 'line 7 : field "i" must be assigned',
    --ref = 'line 5 : invalid attribution (not a reference)',
    --run = 1,
}
Test { [[
var int i = 1;
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
var Tx t;
escape i;
]],
    ref = 'line 8 : missing initialization for field "i" (declared in tests.lua:3)',
    --ref = 'line 8 : field "i" must be assigned',
    --ref = 'line 5 : invalid attribution (not a reference)',
    --run = 1,
}
Test { [[
var int i = 1;
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
var Tx t;
escape t.i;
]],
    ref = 'line 8 : missing initialization for field "i" (declared in tests.lua:3)',
    --ref = 'line 8 : field "i" must be assigned',
    --ref = 'line 5 : invalid attribution (not a reference)',
    --run = 10,
}
Test { [[
var int i = 0;
class Tx with
    var& int i;
do
    i = 10;
end
spawn Tx;
escape i;
]],
    ref = 'line 7 : missing initialization for field "i" (declared in tests.lua:3)',
    --ref = 'line 5 : invalid attribution (not a reference)',
    --ref = 'line 7 : field "i" must be assigned',
    --run = 1,
}
Test { [[
class Tx with
do
end
spawn Tx;
escape 10;
]],
    --ref = 'line 7 : field "i" must be assigned',
    run = 10,
}
Test { [[
var int i = 0;
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
var Tx&&? p = spawn Tx;
escape p!:i;
]],
    ref = 'line 8 : missing initialization for field "i" (declared in tests.lua:3)',
    --ref = 'line 8 : field "i" must be assigned',
    --run = 10,
}
Test { [[
var int i = 0;
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
var Tx t with
    this.i = &outer.i;
end;
escape i;
]],
    --ref = 'line 9 : cannot assign to reference bounded inside the class',
    run = 10,
}
Test { [[
var int i = 1;
class Tx with
    var& int i;
    var int v = 10;
do
    v = i;
end
var Tx t with
    this.i = &outer.i;
end;
escape i;
]],
    --ref = 'line 9 : cannot assign to reference bounded inside the class',
    run = 1,
}
Test { [[
input void OS_START;
var int i = 1;
class Tx with
    var& int i;
    var int v = 10;
do
    await OS_START;
    v = i;
end
var Tx t with
    this.i = &outer.i;
end;
i = 10;
await OS_START;
escape t.v;
]],
    --ref = 'line 9 : cannot assign to reference bounded inside the class',
    run = 10,
}
Test { [[
var int i = 0;
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
spawn Tx with
    this.i = &outer.i;
end;
escape i;
]],
    --ref = 'line 9 : cannot assign to reference bounded inside the class',
    run = 10,
}

Test { [[
var int i = 1;
class Tx with
    var& int? i;
do
    var int v = 10;
    i! = v;
end

var int ret = 0;

var Tx t1;
ret = ret + i;  // 1
spawn Tx;
ret = ret + i;  // 2

var Tx t2 with
    this.i = &outer.i;
end;
ret = ret + i;  // 12

i = 0;
spawn Tx with
    this.i = &i;
end;
ret = ret + i;  // 22

escape ret;
]],
    --ref = 'line 17 : cannot assign to reference bounded inside the class',
    --run = 22,
    asr = ':6] runtime error: invalid tag',
}
Test { [[
var int i = 1;
class Tx with
    var& int? i;
do
    var int v = 10;
    if i? then
        i! = i! + v;
    end
end

var int ret = 0;

var Tx t1;
ret = ret + i;  // 1    1
spawn Tx;
ret = ret + i;  // 1    2

var Tx t2 with
    this.i = &outer.i;
end;
ret = ret + i;  // 11   13

i = 0;
spawn Tx with
    this.i = &i;
end;
ret = ret + i;  // 10   23

escape ret;
]],
    --ref = 'line 17 : cannot assign to reference bounded inside the class',
    run = 23,
}
Test { [[
var int i = 1;
class Tx with
    var& int? i;
do
    if i? then
    end
end
var Tx t with
    this.i = &outer.i;
end;
escape 1;
]],
    run = 1,
}
Test { [[
var int i = 1;
class Tx with
    var& int? i;
    var int  v = 0;
do
    if i? then
        v = 10;
    end
end

var int ret = 0;

var Tx t1;
ret = ret + i;  // 1
spawn Tx;
ret = ret + i;  // 2

var Tx t2 with
    this.i = &outer.i;
end;
ret = ret + t2.v;  // 12

i = 0;
spawn Tx with
    this.i = &i;
end;
ret = ret + t2.v;  // 22

escape ret;
]],
    run = 22,
}

Test { [[
var int i = 1;
var& int v = i;

class Tx with
    var int&& p = null;
    var& int v = null;
do
end

var Tx t with
    this.p = v;
    this.v = &v;
end;

escape *(t.p) + *(t.v);
]],
    env = 'line 6 : types mismatch (`int&´ <= `null&&´)',
}

Test { [[
var int i = 1;
var& int v = &i;

class Tx with
    var int&& p = null;
    var& int v;
do
end

var Tx t with
    this.p = &&v;
    this.v = &v;
end;

escape *(t.p) + (t.v);
]],
    run = 2,
}

Test { [[
var int i = 1;
var& int v = &i;

class Tx with
    var int&& p = null;
    var& int v;
do
    await 1s;
    //v = 1;
    *p = 1;
end

var Tx t with
    this.p := &&v;
    this.v = &v;
end;

escape *(t.p) + (t.v);
]],
    fin = 'line 10 : unsafe access to pointer "p" across `await´',
}

Test { [[
class Tx with
    var _SDL_Rect&& cell_rects = null;
do
    var _SDL_Rect&& cell_rect = &&this.cell_rects[1];
end
escape 1;
]],
    gcc = 'error: unknown type name ‘SDL_Rect’',
}

Test { [[
native _BGS;
native do
    int  vs[] = { 1, 2 };
    int* BGS[] = { &vs[0], &vs[1] };
end
escape *_BGS[1];
]],
    run = 2,
}

Test { [[
native _t;
pre native do
    typedef int* t;
end
var int v = 2;
var _t p = &&v;
escape *p;
]],
    run = 2,
}

Test { [[
native/plain _t;
pre native do
    typedef int t;
end
var _t v = 2;
escape *v;
]],
    env = 'line 6 : invalid operand to unary "*"',
}

Test { [[
native/plain _rect;
pre native do
    typedef struct rect {
        int* x, y;
    } rect;
end
var int v = 10;
var _rect r = _rect(&&v);
escape *(r.x);
]],
    fin = 'line 8 : call requires `finalize´',
}
Test { [[
native/plain _rect;
pre native do
    typedef struct rect {
        int* x, y;
    } rect;
end
var int v = 10;
var _rect r;
do
    r = _rect(&&v);
finalize with nothing; end
escape *(r.x);
]],
    run = 10,
}
Test { [[
native/plain _rect;
pre native do
    typedef struct rect {
        int* x, y;
    } rect;
    int V = 0;
end
do
    var int v = 10;
    var _rect r;
native ___ceu_nothing;
native _V;
do r = _rect(&&v); finalize with _V=v; end;
    ___ceu_nothing(&&r);
end
escape _V;
]],
    run = 10,
}

Test { [[
pre native do
    typedef struct t {
        int* x;
    } t;
end
native/plain _t;
var int v = 10;
var _t t;
do t = _t(&&v); finalize with nothing; end;
await 1s;
escape *t.x;
]],
    run = {['~>1s']=10},
}

Test { [[
input void OS_START;
var int v=0;
class Tx with
    var int v=0;
do
    v = 5;
end
var Tx a;
await OS_START;
v = a.v;
a.v = 4;
escape a.v + v;
]],
    run = 9,
}

Test { [[
input void OS_START;
class Tx with
    var int v=0;
do
    this.v = 5;
end
do
    var Tx a;
    await OS_START;
    var int v = a.v;
    a.v = 4;
    escape a.v + v;
end
]],
    run = 9,
}

Test { [[
input void OS_START, A;
class Tx with
    var int v=0;
do
    await OS_START;
    this.v = 5;
end
do
    var Tx a;
        a.v = 0;
    await A;
    escape a.v;
end
]],
    run = { ['~>A']=5} ,
}

Test { [[
var int sum = 0;
class C with
do
end

par/or do
    loop do
        do
            par/or do
                await FOREVER;
            with
                sum = sum + 1;
                await 1s;
            end
        end
        do
            var C c;
            await 1s;
        end
        // go back to do-end first trail
    end
with
    await 2s;
end
escape sum;
]],
    _ana = {acc=true},
    run = { ['~>10s']=2 },
}

Test { [[
input void OS_START;
class Tx with
    event void go;
    var int v=0;
do
    await go;
    v = 5;
end
do
    var Tx a;
    await OS_START;
    par/and do
        emit a.go;      // 13
    with
        emit a.go;      // 15
    end
    var int v = a.v;
    a.v = 4;
    escape a.v + v;
end
]],
    _ana = {
        acc = 1,
    },
    run = 9,
}

Test { [[
input void OS_START;
class Tx with
    event int a, go, ok;
    var int aa=0;
do
    await go;
    emit a => 100;
    aa = 5;
    emit ok => 1;
end
var Tx aa;
    par/or do
        await OS_START;
        emit aa.go => 1;
    with
        await aa.ok;
    end
escape aa.aa;
]],
    run = 5,
}

-- EMIT / SELF-ABORT
Test { [[
input void KILL;

native do
    int V = 0;
end

class OrgA with
    event void evtA;
    event void evtB;
do
    loop do
        watching evtA do
            loop do
                loop i in [0 |> 3[ do
                    await 1s;
                end
                emit evtB;
native _assert;
                _assert(0);
            end
        end
native _V;
        _V = 10;
    end
end

class OrgB with
do
    var OrgA a;
    
    loop do
        await a.evtB;
        emit a.evtA;
        //_assert(0);
    end
end

var OrgB b;

await KILL;

escape _V;
]],
    run = { ['~>3s;~>KILL']=10 },
}
Test { [[
input void OS_START;
class Tx with
    var int v=0;
do
    v = 5;
end
var Tx a;
    await OS_START;
var int v = a.v;
a.v = 4;
escape a.v + v;
]],
    run = 9,
}

Test { [[
var int ret = 0;
do
    var int a;
    do
        class Tx with
        do
            a = 1;
        end
        var Tx v;
    end
    ret = a;
end
escape ret;
]],
    locs = 'line 7 : internal identifier "a" is not declared',
    --props = 'line 5 : must be in top-level',
}

Test { [[
do
    var int a;
    do
        class Tx with
        do
            a = 1;
        end
    end
end
var Tx v;
emit v.go;
escape 0;
]],
    locs = 'line 6 : internal identifier "a" is not declared',
    --props = 'line 4 : must be in top-level',
}

Test { [[
var int a;
do
    do
        class Tx with
        do
            a = 1;
            b = 1;
        end
    end
end
var int b;
var Tx v;
emit v.go;
escape a;
]],
    locs = 'line 6 : internal identifier "a" is not declared',
    --locs = 'line 6 : internal identifier "b" is not declared',
}

Test { [[
var int a;
var int b;
do
    do
        class Tx with
        do
            a = 1;
            b = 3;
        end
    end
end
do
    var int a;
end
do
    var int b;
    do
        var Tx v;
        emit v.go;
    end
end
escape a+b;
]],
    locs = 'line 7 : internal identifier "a" is not declared',
    --props = 'line 5 : must be in top-level',
    --env = 'line 17 : class "Tx" is not declared',
}

Test { [[
var int a;
var int b;
class Tx with
do
    a = 1;
    b = 3;
end
do
    var int a;
end
do
    var int b;
    do
        var Tx v;
        emit v.go;
    end
end
escape a+b;
]],
    locs = 'line 5 : internal identifier "a" is not declared',
    --run = 4,
}

Test { [[
class Sm with
do
    var u8 id = 0;
end

class Image_media with
    var Sm sm;
do
end

var Image_media img1;
var Image_media img2;

escape 1;
]],
    props = 'line 7 : not permitted inside an interface',
}
Test { [[
class Sm with
do
    var u8 id=0;
    if id then end;
end

class Image_media with
    var Sm&& sm=null;
do
    var Sm smm;
    this.sm = &&smm;
end

var Image_media img1;
var Image_media img2;

escape 1;
]],
    run = 1;
}
Test { [[
class Sm with
    var int id=0;
do
end

class Image_media with
    var Sm sm;
do
end

var Image_media img1;
    img1.sm.id = 10;

var Image_media img2;
    img2.sm.id = 12;

var Image_media img3;
    img3.sm.id = 11;

escape img1.sm.id + img2.sm.id + img3.sm.id;
]],
    props = 'line 7 : not permitted inside an interface',
}
Test { [[
class Sm with
    var int id=0;
do
end

class Image_media with
    var Sm&& sm=null;
do
    var Sm smm;
    this.sm = &&smm;
end

var Image_media img1;
    img1.sm:id = 10;

var Image_media img2;
    img2.sm:id = 12;

var Image_media img3;
    img3.sm:id = 11;

escape img1.sm:id + img2.sm:id + img3.sm:id;
]],
    run = 33;
}
Test { [[
class Sm with
    var int id=0;
do
end

class Image_media with
    var Sm sm;
do
    par do with with with with end
end

var Image_media img1;
    img1.sm.id = 10;

var Image_media img2;
    img2.sm.id = 12;

var Image_media img3;
    img3.sm.id = 11;

escape img1.sm.id + img2.sm.id + img3.sm.id;
]],
    _ana = {
        reachs = 1,
    },
    props = 'line 7 : not permitted inside an interface',
}
Test { [[
class Sm with
    var int id=0;
do
end

class Image_media with
    var Sm&& sm=null;
do
    var Sm smm;
    this.sm = &&smm;
    par do with with with with end
end

var Image_media img1;
    img1.sm:id = 10;

var Image_media img2;
    img2.sm:id = 12;

var Image_media img3;
    img3.sm:id = 11;

escape img1.sm:id + img2.sm:id + img3.sm:id;
]],
    _ana = {
        reachs = 1,
    },
    run = 33;
}

Test { [[
class Tx with
    var int v=0;
do
end

var Tx t;
    t.v = 10;
var Tx&& p = &&t;
escape p:v;
]],
    run = 10,
}

Test { [[
class Tx with
    var int v=0;
do
end

var Tx t1, t2;
t1.v = 1;
t2.v = 2;
escape t1.v+t2.v;
]],
    run = 3,
}

Test { [[
class Tx with
native _char;
    var _char&& ptr;
do
end

var _char&& ptr=null;
var Tx t with
    this.ptr = ptr;
end;
escape 1;
]],
    --gcc = 'may be used uninitialized in this function',
    run = 1,
    --fin = 'line 8 : attribution to pointer with greater scope',
}
Test { [[
class Tx with
native _char;
    var _char&& ptr=null;
do
end

var Tx t with
    do
        var _char&& ptr=null;
        this.ptr = ptr;
    end
end;
escape 1;
]],
    --fin = 'line 9 : attribution to pointer with greater scope',
    --fin = 'line 9 : attribution requires `finalize´',
    --fin = 'line 9 : attribution to pointer with greater scope',
    run = 1,
}

Test { [[
class Tx with
    event void go;
do
end
var Tx aa;
loop do
    emit aa.go;
end
]],
    _ana = {
        isForever = true,
    },
    loop = true,
}

Test { [[
input void OS_START;
input void A,B;
var int v = 0;
class Tx with
    event void e, ok, go;
do
    await A;
    emit e;
    emit ok;
end
var Tx a;
await OS_START;
par/or do
    loop i in [0 |> 3[ do
        par/and do
            emit a.go;
        with
            await a.e;
            v = v + 1;
        with
            await a.ok;
        end
    end
with
    await B;
end
escape v;
]],
    run = { ['~>A;~>A;~>A;~>B']=1 },
}

Test { [[
input void OS_START;
input void A,B;
var int v=0;
class Tx with
    event void e, ok;
do
    await A;
    emit e;
    emit ok;
end
var Tx a;
await OS_START;
par/or do
    loop i in [0 |> 3[ do
        par/and do
            await a.e;
            v = v + 1;
        with
            await a.ok;
        end
    end
with
    await B;
end
escape v;
]],
    run = { ['~>A;~>A;~>A;~>B']=1 },
}

Test { [[
input void OS_START;
class Tx with
do
    await FOREVER;
end
await OS_START;
var Tx a;
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
input void A,B;
var int v=0;
class Tx with
    event void e;
do
    loop do
        await A;
        emit e;
    end
end
await OS_START;
var Tx a;
par/or do
    loop i in [0 |> 3[ do
        await a.e;
        v = v + 1;
    end
with
    await B;
end
escape v;
]],
    run = { ['~>A;~>A;~>A;~>B']=3 },
}

Test { [[
input void OS_START;
input void A,B;
var int v=0;
class Tx with
    event void e;
do
    loop do
        await A;
        emit e;
    end
end
var Tx a;
await OS_START;
loop i in [0 |> 3[ do
    await a.e;
    v = v + 1;
end
escape v;
]],
    run = { ['~>A;~>A;~>A']=3 },
}

Test { [[
input void OS_START;
class Tx with
    event void go, ok;
do
    await go;
    await 1s;
    emit ok;
end
var Tx aa;
par/and do
    await OS_START;
    emit aa.go;
with
    await aa.ok;
end
escape 10;
]],
    run = { ['~>1s']=10 },
}

Test { [[
input void OS_START;
do
class Tx with
    event void go, ok;
do
    await 1s;
    emit ok;
end
end
var Tx aa;
par/and do
    await OS_START;
with
    await aa.ok;
end
escape 10;
]],
    run = { ['~>1s']=10 },
}

Test { [[
input int B;
class Tx with
    var int v = await B;
do
end
escape 0;
]],
    props = 'line 3 : not permitted inside an interface',
}

Test { [[
input void OS_START;
input int B;
do
    class Tx with
        event void ok;
        var int v=0;
    do
        v = await B;
        emit ok;
    end
end
var Tx aa;
par/and do
    await OS_START;
with
    await aa.ok;
end
escape aa.v;
]],
    run = { ['10~>B']=10 },
}

Test { [[
class Tx with
    event void e;
do
end

input void B, OS_START;
var int ret = 0;

var Tx a, b;
par/and do
    await a.e;
    ret = 2;
with
    await OS_START;
    emit a.e;
end
escape ret;
]],
    run = 2,
}

Test { [[
class Tx with
    event void e;
do
end

input void B, OS_START;
var int ret = 0;

var Tx a, b;
par/or do
    par/and do
        await a.e;
        ret = 2;
    with
        await OS_START;
        emit b.e;
    end
with
    await B;
    ret = 1;
end
escape ret;
]],
    run = { ['~>B']=1 }
}

Test { [[
input void OS_START;
input void B;
class T1 with
    event void ok;
do
    loop do
        await 1s;
        emit this.ok;
    end
end
class Tx with
    event void ok;
do
    var T1 a;
    par/and do
        await 1ms;
    with
        await a.ok;
    end
    await 1s;
    emit ok;
end
var int ret = 10;
par/or do
    do
        var Tx aa;
        par/and do
            await OS_START;
        with
            await aa.ok;
        end
    end
    ret = ret + 1;
with
    await B;
end
await B;
escape ret;
]],
    run = {
        --['~>B;~>5s;~>B'] = 10,
        ['~>1s;~>B;~>B;~>1s'] = 10,
        --['~>1s;~>B;~>1s;~>B'] = 10,
        --['~>1s;~>2s;~>B'] = 11,
    },
}

Test { [[
input void OS_START;
input void B;
class T1 with
do
end
class Tx with
do
    var T1 a;
    par/and do
        await FOREVER;
    with
        await FOREVER;
    end
end
var int ret = 10;
var Tx aa;
par/or do
    par/and do
        await OS_START;
    with
        await FOREVER;
    end
with
    await B;
end
escape ret;
]],
    ana = 'line 10 : trail should terminate',
    run = {
        ['~>B'] = 10,
    },
}
Test { [[
input void OS_START;
input void B;
class T1 with
do
end
class Tx with
do
    var T1 a;
    par/and do
        await FOREVER;
    with
        await FOREVER;
    end
end
var int ret = 10;
var Tx aa;
par/or do
    par/and do
        await OS_START;
    with
        await FOREVER;
    end
with
    await B;
end
escape ret;
]],
    wrn = true,
    run = {
        ['~>B'] = 10,
    },
}

Test { [[
input void OS_START;
input void B;
class T1 with
    event void ok;
do
    await 1s;
    emit ok;
    await FOREVER;
end
class Tx with
    event void ok;
do
    var T1 a;
    par/and do
        await (0)ms;
    with
        await a.ok;
    end
    await 1s;
    emit ok;
    await FOREVER;
end
var int ret = 10;
var Tx aa;
par/or do
    do
        par/and do
            await OS_START;
        with
            await aa.ok;
        end
    end
    ret = ret + 1;
with
    await B;
end
await B;
escape ret;
]],
    run = {
        ['~>B;~>5s;~>B'] = 10,
        ['~>1s;~>B;~>B;~>1s'] = 10,
        ['~>1s;~>B;~>1s;~>B'] = 10,
        ['~>1s;~>1s;~>B'] = 11,
    },
}

Test { [[
input void E,B;

class Tx with
do
    await E;
end
var int ret = 10;
par/or do
    var Tx aa;
    await FOREVER;
with
    await B;
    ret = 5;
end
escape ret;
]],
    run = {
        ['~>B;~>E'] = 5,
        ['~>E;~>B'] = 5,
    },
}

Test { [[
input void E,B;

class Tx with
do
    par/or do
        await E;
    with
        await 1s;
    end
end
var int ret = 10;
par/or do
    var Tx aa;
    await FOREVER;
with
    await B;
    ret = 5;
end
escape ret;
]],
    run = {
        ['~>B;~>1s;~>E'] = 5,
        ['~>E;~>1s;~>B'] = 5,
    },
}

Test { [[
input void OS_START;
input void B;
class Tx with
    event void ok;
do
    input void E;
    par/or do
        await 1s;
    with
        await E;
    end
    await 1s;
    emit ok;
end
var int ret = 10;
par/or do
    do
        var Tx aa;
        await aa.ok;
    end
    ret = ret + 1;
with
    await B;
end
await B;
escape ret;
]],
    run = {
        --['~>B;~>5s;~>B'] = 10,
        ['~>1s;~>B;~>B;~>1s'] = 10,
        --['~>1s;~>B;~>1s;~>B'] = 10,
        --['~>1s;~>1s;~>B'] = 11,
        --['~>1s;~>E;~>1s;~>B'] = 11,
    },
}

Test { [[
input void OS_START;
input void B;
native _V;
native do
    int V = 0;
end
class T1 with
    event void ok;
do
    await 1s;
    _V = _V + 2;
    emit ok;
    _V = _V + 1000;
end
class Tx with
    event void ok;
do
    do
        var T1 a;
        await a.ok;
        _V = _V * 2;
    end
    await 1s;
    _V = _V + 1;
end
do
    var Tx aa;
    await B;
    _V = _V * 2;
end
escape _V;
]],
    run = {
        ['~>B;~>5s;~>B'] = 0,
        ['~>1s;~>B;~>B;~>1s'] = 8,
        ['~>1s;~>B;~>1s;~>B'] = 8,
        ['~>1s;~>1s;~>B'] = 10,
    },
    --run = { ['~>1s']=0 },
}
Test { [[
input void OS_START;
input void B;
class T1 with
    event void ok;
do
    await 1s;
    emit ok;
end
class Tx with
    event void ok;
do
    input void E;
    par/or do
        var T1 a;
        par/and do
            await (0)ms;
        with
            await a.ok;
        end
    with
        await E;
    end
    await 1s;
    emit ok;
end
var int ret = 10;
par/or do
    do
        var Tx aa;
        await aa.ok;
    end
    ret = ret + 1;
with
    await B;
end
await B;
escape ret;
]],
    run = {
        ['~>B;~>5s;~>B'] = 10,
        ['~>1s;~>B;~>B;~>1s'] = 10,
        ['~>1s;~>B;~>1s;~>B'] = 10,
        ['~>1s;~>1s;~>B'] = 11,
        ['~>1s;~>E;~>1s;~>B'] = 11,
    },
    --run = { ['~>1s']=0 },
}

Test { [[
input void OS_START;
input void B;
class Tx with
    event void ok;
do
    await 1s;
    emit ok;
end
var Tx aa;
var int ret = 10;
par/or do
    par/and do
        await OS_START;
    with
        await aa.ok;
    end
    ret = ret + 1;
with
    await B;
end
await B;
escape ret;
]],
    run = {
        ['~>1s;~>B'] = 11,
        ['~>B;~>1s;~>B'] = 10,
    },
}

Test { [[
input void OS_START;
input void B;
class Tx with
    event void ok;
do
    loop do
        await 1s;
        emit this.ok;       // 8
    end
end
var Tx aa;
var int ret = 0;
par/or do
    loop do
        par/and do
            await (0)ms;
        with
            await aa.ok;    // 18
        end
        ret = ret + 1;
    end
with
    await B;
end
escape ret;
]],
    _ana = {
        --acc = 1,  -- TODO
    },
    run = { ['~>5s;~>B'] = 5 },
}

Test { [[
input void A;
class Tx with
    var int v=0;
do
    await A;
    v = 1;
end
var Tx a;
await A;
a.v = 2;
escape a.v;
]],
    _ana = {
        --acc = 2,    -- TODO
    },
    run = { ['~>A']=2 },
}

Test { [[
input void A;
class Tx with
    var int v=0;
    event void ok;
do
    v = 0;
    loop do
        await A;
        v = v + 1;      // 9
    end
end
var Tx aa;
par do
    await aa.ok;
with
    await A;
    if aa.v == 3 then   // 17
        escape aa.v;    // 18
    end
end
]],
    _ana = {
        --acc = 2,      -- TODO
        reachs = 1,
    },
}

Test { [[
input void OS_START;
input void A;
class Tx with
    event int a, ok;
    var int aa=0;
do
    par/or do
        await A;
        emit a => 10;
        this.aa = 5;
    with
        aa = await a;
        aa = 7;
    end
    emit ok => 1;
end
var Tx aa;
par/and do
    await OS_START;
with
    await aa.ok;
end
escape aa.aa;
]],
    run = { ['~>A']=7 },
}

Test { [[
input void OS_START;
input void A;
class Tx with
    event int a, ok;
    var int aa=0;
do
    par/or do
        await A;
        emit a => 10;
        this.aa = 5;
    with
        aa = await a;
        aa = 7;
    end
    emit ok => 1;
end
var Tx aa;
par/and do
    await OS_START;
with
    await aa.ok;
end
escape aa.aa;
]],
    run = { ['~>A']=7 },
    safety = 2,
    _ana = {
        acc = 2,
    },
}

Test { [[
input void OS_START;
class Tx with
    event int a;
    var int aa=0;
do
    par/and do
        emit this.a => 10; // 6
        aa = 5;
    with
        await a;        // 9
        aa = 7;
    end
end
var Tx aa;
await OS_START;
escape aa.aa;
]],
    _ana = {
        acc = 1,
    },
    run = 5,
}

Test { [[
input void A,B;
var int a = 0;
do
    var u8 a = 1;
    if a then end;
end
par/and do
    await A;
    a = a + 1;
with
    await B;
    a = a + 1;
end
escape a;
]],
    wrn = true,
    run = { ['~>B;~>A']=2 },
}

Test { [[
class Tx with do end
var Tx a;
var Tx&& p = a;
]],
    env = 'line 3 : types mismatch',
}

Test { [[
class Tx with do end;
do
    var int ret = 1;
    var Tx t;
    escape ret;
end
]],
    run = 1,
}

Test { [[
class Tx with do end;
do
    var Tx t;
    var int ret = 1;
    escape ret;
end
]],
    run = 1,
}

Test { [[
class Tx with do end;
do
    var int a = 1;
    var int&& pa = &&a;
    var Tx t;
    var int ret = *pa;
    escape ret;
end
]],
    run = 1,
}

Test { [[
native _c, _d;
native do
    int c, d;
end

class Tx with
    var int a=0;
do
end

var int i;
i = 10;
var int&& pi = &&i;

var Tx t;
t.a = 10;
var Tx&& p = &&t;
_c = t.a;
_d = p:a;
escape p:a + t.a + _c + _d;
]],
    run = 40,
}

Test { [[
input void A, B;

var s16 x = 10;

par/or do
    loop do
        par/or do
            loop do
                await 10ms;
                x = x + 1;
            end
        with
            await B;
        end
        par/or do
            loop do
                await 10ms;
                x = x - 1;
            end
        with
            await B;
        end
    end
with
    await A;
with
    async do
        emit B;
        emit 10ms;
        emit A;
    end
end
escape x;
]],
    run = 9,
}

Test { [[
class Tx with
    var int a;
do
end

var int i = 1;
vector[2] Tx y with
    this.a = 10*i;
    i = i + 1;
end;

var Tx x with
    this.a = 30;
end;

escape x.a + y[0].a + y[1].a;
]],
    run = 60,
}

Test { [[
class Tx with
    var int a;
do
end

var int i = 0;

vector[2] Tx y with
    i = i + 1;
    this.a = i*10;
end;

var Tx x with
    this.a = 30;
end;

escape x.a + y[0].a + y[1].a;
]],
    run = 60,
}

Test { [[
native do
    int V = 0;
end
native _V;

class Tx with
do
    _V = _V + 1;
end

var Tx ts;

escape _V;
]],
    run = 1,
}

Test { [[
native do
    int V = 0;
end
native _V;
class Tx with
do
    _V = _V + 1;
end

vector[20000] Tx ts;

escape _V;
]],
    run = 20000,
}

Test { [[
class Tx with
do
    class T1 with var int v=0; do end
    var int v=0;
    if v then end;
end
escape 0;
]],
    run = 0, -- TODO
    --props = 'line 2 : must be in top-level',
}

Test { [[
class Tx with
do
end
vector[5] Tx a;
escape 0;
]],
    run = 0,
}

Test { [[
native/const _U8_MAX;
class Tx with do end;
vector[_U8_MAX] Tx ts;

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
    var int a = 1;
    if a then end;
end
vector[2] Tx ts;
escape 1;
]],
    run = 1,
}
Test { [[
class Tx with
    var int a=0;
do
end
vector[2] Tx ts;
par/and do
    ts[0].a = 10;   // 7
with
    ts[1].a = 20;   // 9
end
escape ts[0].a + ts[1].a;
]],
    _ana = {
        acc = 1,
    },
    run = 30,
}
Test { [[
class Tx with
    var int a=0;
do
end
var Tx t1, t2;
par/and do
    t1.a = 10;   // 7
with
    t2.a = 20;   // 9
end
escape t1.a + t2.a;
]],
    run = 30,
}
Test { [[
input void OS_START;
class Tx with
    var int a=0;
do
    await OS_START;
    a = 0;
end
vector[2] Tx ts;
await OS_START;
par/and do
    ts[0].a = 10;   // 11
with
    ts[1].a = 20;   // 13
end
escape ts[0].a + ts[1].a;
]],
    _ana = {
        acc = 1,  -- TODO=5?
    },
    run = 30,
}
Test { [[
input void OS_START;
class Tx with
    var int a=0;
do
    await OS_START;
    this.a = 0;
end
vector[2] Tx ts;
await OS_START;
par/and do
    ts[0].a = 10;
with
    ts[1].a = 20;
end
escape ts[0].a + ts[1].a;
]],
    _ana = {
        acc = 1,    -- TODO: 5?
    },
    run = 30,
}
Test { [[
input void OS_START;
class Tx with
    var int a=0;
do
    await OS_START;
    a = 0;
end
var Tx t1, t2;
await OS_START;
par/and do
    t1.a = 10;
with
    t2.a = 20;
end
escape t1.a + t2.a;
]],
    _ana = {
        --acc = 8,      -- TODO
    },
    run = 30,
}
Test { [[
input void OS_START;
class Tx with
    var int a=0;
do
    await OS_START;
    this.a = 0;
end
var Tx t1, t2;
await OS_START;
par/and do
    t1.a = 10;
with
    t2.a = 20;
end
escape t1.a + t2.a;
]],
    _ana = {
        --acc = 8,  -- TODO
    },
    run = 30,
}
Test { [[
input void OS_START;
native/nohold _f;
native do
    void f (void* t) {}
end
class Tx with
do
    await OS_START;
    _f(&&this);       // 9
end
vector[2] Tx ts;
await OS_START;
par/and do
    _f(&&ts[0]);     // 14
with
    _f(&&ts[1]);     // 16
end
escape 10;
]],
    _ana = {
        acc = 2,
        --acc = 6,    -- TODO: not checked
    },
    run = 10,
}
Test { [[
input void OS_START;
native/nohold _f;
native do
    void f (void* t) {}
end
class Tx with
do
    await OS_START;
    _f(&&this);       // 9
end
var Tx t0,t1;
await OS_START;
par/and do
    _f(&&t0);     // 14
with
    _f(&&t1);     // 16
end
escape 10;
]],
    _ana = {
        acc = 1,
        --acc = 9,  -- TODO
    },
    run = 10,
}

Test { [[
native do ##include <assert.h> end
native _assert;
native _assert;
input int  BUTTON;
input void B;

class Rect with
    var s16 x=0;
    var s16 y=0;
    event void go;
do
    loop do
        par/or do
            loop do
                await 10ms;
                x = x + 1;
            end
        with
            await go;
        end
        par/or do
            loop do
                await 10ms;
                y = y + 1;
            end
        with
            await go;
        end
        par/or do
            loop do
                await 10ms;
                x = x - 1;
            end
        with
            await go;
        end
        par/or do
            loop do
                await 10ms;
                y = y - 1;
            end
        with
            await go;
        end
    end
end

vector[2] Rect rs;
rs[0].x = 10;
rs[0].y = 50;
rs[1].x = 100;
rs[1].y = 300;

par/or do
    loop do
        var int i = await BUTTON;
        emit rs[i].go;
    end
with
    await B;
with
    async do
        emit 100ms;
    end
native _assert;
    _assert(rs[1].x==110);
    _assert(rs[0].x==20 and rs[0].y==50 and rs[1].x==110 and rs[1].y==300);

    async do
        emit BUTTON => 0;
        emit 100ms;
    end
    _assert(rs[0].x==20 and rs[0].y==60 and rs[1].x==120 and rs[1].y==300);

    async do
        emit BUTTON => 1;
        emit 100ms;
    end
    _assert(rs[0].x==20 and rs[0].y==70 and rs[1].x==120 and rs[1].y==310);

    async do
        emit BUTTON => 1;
        emit 100ms;
    end
    _assert(rs[0].x==20 and rs[0].y==80 and rs[1].x==110 and rs[1].y==310);

    async do
        emit BUTTON => 1;
        emit 99ms;
    end
    _assert(rs[0].x==20 and rs[0].y==89 and rs[1].x==110 and rs[1].y==301);

    async do
        emit BUTTON => 0;
        emit 1ms;
    end
    _assert(rs[0].x==20 and rs[0].y==89 and rs[1].x==110 and rs[1].y==300);

    async do
        emit 18ms;
    end
    _assert(rs[0].x==19 and rs[0].y==89 and rs[1].x==110 and rs[1].y==299);

    async do
        emit BUTTON => 0;
        emit BUTTON => 1;
        emit 1s;
    end
    _assert(rs[0].x==19 and rs[0].y==-11 and rs[1].x==210 and rs[1].y==299);

end
escape 100;
]],
    awaits = 0,
    run = 100,
}

Test { [[
class Tx with
    event int a, go, ok;
    var int aa=0;
do
    par/or do
        emit a => 10;      // 5
        aa = 5;
    with
        await this.a;   // 8
        aa = 7;
    end
end
var Tx aa;
par/or do
    par/and do
        emit aa.go => 1;
    with
        await aa.ok;
    end
with
    input void OS_START;
    await OS_START;
end
escape aa.aa;
]],
    _ana = {
        acc = 1,
    },
    run = 5,
}

Test { [[
class Tx with
    event int a, ok, go;
    var int aa=0;
do
    emit a => 10;
    aa = 5;
end
var Tx aa;
par/or do
    par/and do
        emit aa.go => 1;
    with
        await aa.ok;
    end
with
    input void OS_START;
    await OS_START;
end
escape aa.aa;
]],
    run = 5,
}

Test { [[
input void OS_START;

native _inc, _V;
native do
    int V = 0;
    void inc() { V++; }
end

_inc();
event void x;
emit x;
await OS_START;
escape _V;
]],
    run = 1,
}

Test { [[
input void OS_START;

native _inc, _V;
native do
    int V = 0;
    void inc() { V++; }
end

class Tx with
    event void a, ok, go, b;
    var int aa=0, bb=0;
do
    par/and do
        await a;            // 3.
        emit b;             // 4.
    with
        await b;            // 5.
    end
    aa = 5;                 // 6. V=1, aa=5
    bb = 4;                 // 7. V=1, aa=5, bb=4
    emit ok;                // 8.
end

var Tx aa;

_inc();                     // 1. V=1
par/or do
    await aa.ok;            // 9.
    _V = _V+1;              // 10. V=2, aa=5, bb=4
with
    await OS_START;
    emit aa.a;              // 2.
    _V = _V+2;
end
escape _V + aa.aa + aa.bb;
]],
    run = 11,
}

Test { [[
    input void OS_START;
class Tx with
    event void a, ok, go, b;
    var int aa=0, bb=0;
do

    par/and do
        await a;
        emit b;
    with
        await b;
    end
    aa = 5;
    bb = 4;
    emit ok;
end
var Tx aa;

native _inc, _V;
native do
    int V = 0;
    void inc() { V++; }
end

_inc();
par/or do
    await aa.ok;
    _V = _V+1;
with
    await OS_START;
    emit aa.a;
    _V = _V+2;
end
escape _V + aa.aa + aa.bb;
]],
    run = 11,
    safety = 2,
    _ana = {
        acc = 3,
    },
}

Test { [[
input void OS_START;
class Tx with
    event void a, ok, go, b;
    var int aa=0, bb=0;
do

    par/and do
        await a;
        emit b;
    with
        await b;
    end
    aa = 5;
    bb = 4;
    emit ok;
end
var Tx aa;

var int ret=0;
par/or do
    await aa.ok;
    ret = 1;
with
    await OS_START;
    emit aa.a;
    ret = 2;
end
escape ret + aa.aa + aa.bb;
]],
    run = 10,
}

Test { [[
input void OS_START;
class Tx with
    event void e, ok, go;
    var int ee=0;
do
    await this.go;
    if ee == 1 then
        emit this.e;
    end
    await (0)ms;
    emit ok;
end
var Tx a1, a2;
var int ret = 0;
await OS_START;

par/or do
    par/and do
        a1.ee = 1;
        emit a1.go;
        await a1.ok;
        ret = 1;        // 20
    with
        a2.ee = 2;
        emit a2.go;
        await a2.ok;
        ret = 1;        // 25
    end
with
    await a2.e;
    ret = 100;
end
escape ret;
]],
    _ana = {
        --acc = 1,
    },
    run = { ['~>1s']=1 },
}
Test { [[
native/nohold _f;
input void OS_START;
class Tx with
    event void e, ok, go, b;
    var u8 a=0;
do
    await go;
    a = 1;
    emit ok;
end
var Tx a, b;
native do
    int f (byte* a, byte* b) {
        escape *a + *b;
    }
end
par/and do
    await OS_START;
    emit a.go;
with
    await a.ok;
with
    await OS_START;
    emit b.go;
with
    await b.ok;
end
escape _f(&&a.a as byte&&,&&b.a as byte&&);
]],
    run = 2,
}

Test { [[
input void OS_START;
class Tx with
    event void e, f;
do
    await e;
    emit f;
end
vector[2] Tx ts;
par/and do
    await OS_START;
    emit ts[0].e;
    emit ts[1].e;
with
    await ts[1].f;
end
escape 10;
]],
    run = 10,
}

Test { [[
input void OS_START, B;
class Tx with
    var int v=0;
    event void ok, go, b;
    event void e, f;
do
    await go;
    v = 10;
    await e;
    emit f;
    v = 100;
    emit ok;
end
vector[2] Tx ts;
var int ret = 0;
par/and do
    par/and do
        await OS_START;
        emit ts[0].go;
    with
        await ts[0].ok;
    end
    ret = ret + 1;
with
    par/and do
        await OS_START;
        emit ts[1].go;
    with
        await ts[1].ok;
    end
    ret = ret + 1;
with
    await B;
    emit ts[0].e;
    ret = ret + 1;
with
    await ts[0].f;
    ret = ret + 1;
with
    await B;
    emit ts[1].e;
    ret = ret + 1;              // 42
with
    await ts[1].f;
    ret = ret + 1;              // 45
end
escape ret + ts[0].v + ts[1].v;
]],
    _ana = {
        --acc = 47,     -- TODO: not checked
        acc = 8,
    },
    run = { ['~>B']=206, }
}

Test { [[
input int S;
input void B;
class Tx with
    event void a,ok;
    var int aa=0;
do
    par/or do
        aa = await S;
        emit this.a;
    with
        await 10s;
        await a;
        aa = 7;
    end
    emit ok;
end
var Tx aa;
await aa.ok;
await B;
escape aa.aa;
]],
    run = {
        ['11~>S;~>10s;~>B'] = 11,
        ['~>10s;11~>S;~>B'] = 7,
    },
}

Test { [[
input void OS_START;

class Tx with
    event void ok;
do
    await OS_START;
    emit ok;
end

var int ret = 10;
var Tx t;
await t.ok;

escape ret;
]],
    run = 10,
}

Test { [[
input void OS_START;

class Tx with
    event void ok;
do
    await OS_START;
    emit ok;
end

pool[] Tx ts;
var Tx&&? t1 = spawn Tx in ts;

await t1!:ok;

escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;

class Tx with
    event void ok;
do
    await OS_START;
    emit ok;
end

pool[] Tx ts;
var Tx&&? t1 = spawn Tx in ts;
var Tx&&? t2 = spawn Tx in ts;

par/and do
    await t1!:ok;
with
    await t2!:ok;
end

escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;

class Tx with
do
    await OS_START;
end

pool[] Tx ts;
var Tx&&? t1 = spawn Tx in ts;
var Tx&&? t2 = spawn Tx in ts;

par/and do
    await *t1!;
with
    await *t2!;
end

escape 1;
]],
    _ana = {
        acc = 0,
    },
    run = 1,
}
Test { [[
input void OS_START;

class Tx with
    event void ok;
do
    await OS_START;
    emit ok;
end

var Tx t1, t2;

par/and do
    await t1.ok;
with
    await t2.ok;
end

escape 1;
]],
    _ana = {
        acc = 0,
    },
    run = 1,
}
Test { [[
input void OS_START;

class Tx with
    event void ok;
do
    await OS_START;
    emit ok;
end

vector[2] Tx ts;

par/and do
    await ts[0].ok;
with
    await ts[1].ok;
end

escape 1;
]],
    _ana = {
        acc = 0,
    },
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
    var int v=0;
    event void e, f, ok;
do
    v = 10;
    await e;
    await (0)s;
    emit f;
    v = 100;
    emit ok;
end
vector[2] Tx ts;
var int ret = 0;
par/and do
    par/and do
        await ts[0].ok;
    with
        await ts[1].ok;
    end
    ret = ret + 1;
with
    await OS_START;
    emit ts[0].e;
    ret = ret + 1;
with
    await ts[0].f;
    ret = ret + 1;
with
    await OS_START;
    emit ts[1].e;
    ret = ret + 1;
with
    await ts[1].f;
    ret = ret + 1;
end
escape ret + ts[0].v + ts[1].v;
]],
    _ana = {
        acc = 4,
        --acc = 13,     -- TODO: not checked
    },
    run = { ['~>1s']=205, }
}

Test { [[
input void OS_START;
class Tx with
    var int v=0;
do
    v = 1;
end
var Tx a, b;
await OS_START;
escape a.v + b.v;
]],
    run = 2,
}

Test { [[
input void OS_START;
input void B;
class T1 with
    event void ok;
do
    await 1s;
    emit ok;
end
class Tx with
    event void ok;
do
    var T1 a;
    await a.ok;
    await 1s;
    emit ok;
end
var int ret = 10;
await OS_START;
par/or do
    do
        var Tx aa;
        await aa.ok;
    end
    ret = ret + 1;
with
    await B;
end
await B;
escape ret;
]],
    run = {
        ['~>B;~>5s;~>B'] = 10,
        ['~>1s;~>B;~>B;~>1s'] = 10,
        ['~>1s;~>B;~>1s;~>B'] = 10,
        ['~>1s;~>1s;~>B'] = 11,
    },
}

Test { [[
native _V;
native do
    int V=1;
end

class Tx with
    event void a;
do
    loop do
        await a;
        _V = _V + 1;
    end
end

var Tx t;
emit t.a;
emit t.a;
emit t.a;
escape _V;
]],
    --run = 4,
    run = 1,
}

Test { [[
native _V;
native do
    static int V = 0;
end
do
    do
        do
            do finalize with
                _V = 100;
            end
        end
    end
end
escape _V;
]],
    run = 100;
}

Test { [[
native _V;
native do
    static int V = 0;
end
input void B;
class Tx with
    // nothing
do
    do
        do finalize with
            _V = 100;
        end
        await B;
    end
end
do
    var Tx t;
    input void OS_START;
    await OS_START;
end
escape _V;
]],
    run = 100,
}

Test { [[
native _V;
native do
    static int V = 1;
end
input void B;
class Tx with
do
    _V = 10;
    do
        do finalize with
            _V = _V + 100;
        end
        await B;
    end
end
par/or do
    var Tx t;
    await B;
with
    // nothing;
end
escape _V;
]],
    run = 110,      -- TODO: stack change
}

Test { [[
native _V;
native do
    static int V = 0;
end
input void OS_START;
class Tx with
    // nothing
do
    do
        do finalize with
            _V = 100;
        end
        await OS_START;
    end
end
par/or do
    var Tx t;
    await OS_START;
with
    await OS_START;
end
escape _V;
]],
    _ana = {
        abrt = 1,
    },
    run = 100,
}

Test { [[
native _V;
input void A, B, OS_START;
native do
    int V = 0;
end
class Tx with
    event void e, ok;
    var int v=0;
do
    do finalize with
        _V = _V + 1;        // * writes after
    end
    v = 1;
    await A;
    v = v + 3;
    emit e;
    emit ok;
end
var Tx t;
await OS_START;
par/or do
    do                  // 22
        do finalize with
            _V = _V*10;
        end
        await t.ok;
    end
with
    await t.e;          // 29
    t.v = t.v * 3;
with
    await B;
    t.v = t.v * 5;
end
escape t.v + _V;        // * reads before
]],
    _ana = {
        abrt = 1,        -- false positive
    },
    run = {
        ['~>B'] = 5,
        ['~>A'] = 12,
    }
}

-- internal binding binding
Test { [[
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
var Tx t;
escape t.i;
]],
    ref = 'line 7 : missing initialization for field "i" (declared in tests.lua:2)',
    --ref = 'line 7 : field "i" must be assigned',
    --run = 10,
}

-- internal/constr binding
Test { [[
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
var int v = 0;
var Tx t with
    this.i = &v;
end;
escape v;
]],
    --ref = 'line 9 : cannot assign to reference bounded inside the class',
    run = 10;
}
-- internal binding
Test { [[
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
var Tx t;
escape t.i;
]],
    ref = 'line 7 : missing initialization for field "i" (declared in tests.lua:2)',
    --ref = 'line 7 : field "i" must be assigned',
    --run = 10,
}
-- internal binding w/ default
Test { [[
class Tx with
    var& int? i;
do
    var int v = 10;
    i! = v;
end
var Tx t;
escape t.i!;
]],
    asr = '5] runtime error: invalid tag',
    --run = 10,
}
-- internal binding w/ default
Test { [[
class Tx with
    var& int? i;
do
native _assert;
    _assert(not i?);
    var int v = 10;
    i! = v;
end
var Tx t;
escape t.i!;
]],
    asr = '6] runtime error: invalid tag',
    --run = 10,
}
-- external binding w/ default
Test { [[
class Tx with
    var& int? i;
do
native _assert;
    _assert(i?);
end
var int i = 10;
var Tx t with
    this.i = &outer.i;
end;
escape t.i!;
]],
    run = 10,
}
Test { [[
class Tx with
    var& int? i;
do
native _assert;
    _assert(not i?);
end
var int i = 10;
var Tx t;
escape not t.i?;
]],
    run = 1,
}

-- no binding
Test { [[
class Tx with
    var& int i;
do
end
var Tx t;
escape 1;
]],
    ref = 'line 5 : missing initialization for field "i" (declared in tests.lua:2)',
    --ref = 'line 5 : field "i" must be assigned',
}

Test { [[
class Tx with
    var& int i;
do
end

var int i = 1;

var Tx t1;

var Tx t2 with
    this.i = &outer.i;
end;

escape t1.i;
]],
    ref = 'line 8 : missing initialization for field "i" (declared in tests.lua:2)',
    --ref = 'line 8 : field "i" must be assigned',
}

Test { [[
class Tx with
    var& int i;
do
end

var int i = 1;

var Tx t2 with
    this.i = &outer.i;
end;

var Tx t1;

escape t1.i;
]],
    ref = 'line 12 : missing initialization for field "i" (declared in tests.lua:2)',
    --ref = 'line 12 : field "i" must be assigned',
}

Test { [[
class Tx with
    var& int i;
do
    var int v = 10;
    i = v;
end
var Tx t;
var int v = 0;
t.i = v;
escape 1;
]],
    ref = 'line 7 : missing initialization for field "i" (declared in tests.lua:2)',
    --ref = 'line 9 : cannot assign to reference bounded inside the class',
}

Test { [[
class Integral with
    var&   int v;
    event int  e;
do
    every dv in e do
        v = v + dv;
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
input (_WorldObjs__LaserExit&&, _FileReader&&) LASEREXIT_NEW;
class LaserExitFactory with
do
    every me_ in LASEREXIT_NEW do
        spawn LaserExit with
            this.me = &_XXX_PTR2REF(me_);
        end;
    end
end

var LaserExitFactory x;
]],
    env = 'line 4 : arity mismatch',
}

Test { [[
interface Global with
    var& int v;
end
var int  um = 1;
var& int v;// = um;
escape global:v;
]],
    ref = 'line 6 : invalid access to uninitialized variable "v" (declared at tests.lua:2)',
    --ref = 'line 5 : missing initialization for global variable "v"',
    --ref = 'line 5 : global references must be bounded on declaration',
}

Test { [[
interface Global with
    var& int v;
end
var int  um = 1;
var& int v = &um;
escape 1;//global:v;
]],
    run = 1,
}

Test { [[
interface Global with
    var& int v;
end
var int  um = 1;
var& int v = &um;
escape global:v;
]],
    run = 1,
}

Test { [[
interface Global with
    var& int v;
end

var int  um = 111;
var& int v = &um;

class Tx with
    var int v=0;
do
    this.v = global:v;
end

var Tx t;
escape t.v;
]],
    run = 111,
}

Test { [[
interface Global with
end
var& int? win;
if win? then end;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var int v = 10;
do
end

interface Global with
    var& Tx t;
end

var Tx t_;
var& Tx t = &t_;

escape global:t.v;
]],
    run = 10,
}
Test { [[
class Tx with
    var int v = 10;
do
end

interface Global with
    var& Tx t;
end

var Tx t_;
var& Tx t = &t_;
global:t = &t;

escape global:t.v;
]],
    ref = 'line 12 : invalid attribution : variable "t" is already bound',
}

Test { [[
class Tx with
    event void e;
do
end
var Tx t;

class U with
    var& Tx t;
do
    emit t.e;
end

var U u with
    this.t = &t;
end;

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var int x=0;
do
end
var Tx t;

class U with
    var& Tx t;
do
    t.x = 1;
end

class V with
    var& U u;
do
    u.t.x = 2;
end

var U u with
    this.t = &t;
end;

var V v with
    this.u = &u;
end;

escape t.x + u.t.x + v.u.t.x;
]],
    run = 6,
}

Test { [[
class Tx with
    var int x=0;
do
end
var Tx t;

class U with
    var& Tx t;
do
    t.x = 1;
end

class V with
    var& U u;
do
    var U&& p = &&u;
    p:t.x = 2;
end

var U u with
    this.t = &t;
end;

var V v with
    this.u = &u;
end;

escape t.x + u.t.x + v.u.t.x;
]],
    run = 6,
}

Test { [[
class Ship with
    var& int v;
do
end

loop do
    var int x = 10;
    var Ship ship1 with
        this.v = &x;
    end;
    escape 1;
end
]],
    wrn = true,
    run = 1,
}

Test { [[
class Tx with
    var& int v;
do
end
var Tx t with
    var int x;
    this.v = &x;
end;
escape 1;
]],
    ref = 'line 7 : invalid access to uninitialized variable "x" (declared at tests.lua:6)',
}
Test { [[
class Tx with
    var& int v;
do
end
var Tx t with
    var int x=0;
    this.v = &x;
end;
escape 1;
]],
    ref = 'line 7 : invalid attribution : variable "x" has narrower scope than its destination',
}

Test { [[
class Test with
    var& u8 v;
do
    var int x = v;
end

var& u8 v;

do Test with
    this.v = &v;
end;

escape 1;
]],
    ref = 'line 10 : invalid access to uninitialized variable "v" (declared at tests.lua:7)',
    --ref = 'line 7 : uninitialized variable "v"',
    --run = 1,
}
Test { [[
class Test with
    vector&[10] u8 v;
do
    v = [] .. v .. [4];
end

vector&[10] u8 v; // error: '&' must be deleted

do Test with
    this.v = &v;
end;

escape 1;
]],
    ref = 'line 10 : invalid access to uninitialized variable "v" (declared at tests.lua:7)',
    --ref = 'line 7 : uninitialized variable "v"',
    --run = 1,
}
Test { [[
class Tx with
    var& int v;
do
end
var int x = 10;
var Tx t with
    this.v = &x;
end;
x = 11;
escape t.v;
]],
    run = 11;
}

Test { [[
data Vv with
    var int v;
end

class Tx with
    var& Vv v;
do
end

var Tx t1 with
    var Vv v_ = Vv(1);
    this.v = &v_;
end;
var Tx t2 with
    var Vv v_ = Vv(2);
    this.v = &v_;
end;
var Tx t3 with
    var Vv v_ = Vv(3);
    this.v = &v_;
end;

escape t1.v.v + t2.v.v + t3.v.v;
]],
    ref = 'line 12 : invalid attribution : variable "v_" has narrower scope than its destination',
    --ref = 'line 12 : attribution to reference with greater scope',
    --run = 6,
}

Test { [[
class Tx with
    var& int v;
do
end
var int x = 10;
var Tx t with
    this.v = &x;
end;
var int y = 15;
t.v = y;
y = 100;
escape t.v + x + y;
]],
    run = 130,
}

-- KILL THEMSELVES

Test { [[
native do ##include <assert.h> end
input void OS_START;

interface Global with
    event void e;
end

event void e;

class Tx with
do
    await OS_START;
    emit global:e; // TODO: must also check if org trail is active
    native _assert;
    _assert(0);
end

do
    var Tx t;
    await e;
end
escape 2;
]],
    run = 2,
}
Test { [[
input void OS_START;

native _V, _assert;
native do
    ##include <assert.h>
    int V = 0;
end

interface Global with
    event void e;
end

event void e;

class Tx with
do
    await OS_START;
    emit global:e;
    _V = 1;
    _assert(0);
end

par/or do
    await global:e;
    _V = 2;
with
    var Tx t;
    await FOREVER;
end

escape _V;
]],
    run = 2,
}

Test { [[
input void OS_START;

native _V, _assert;
native do
    ##include <assert.h>
    int V = 0;
end

interface Global with
    event void e;
end

class Tx with
do
    emit global:e; // TODO: must also check if org trail is active
    _V = 1;
    _assert(0);
end

event void e;

par/or do
    await global:e;
    _V = 2;
with
    await OS_START;
    do
        var Tx t;
        await FOREVER;
    end
end
escape _V;
]],
    run = 2;
}

Test { [[
input void OS_START;

native _V, _assert;
native do
    ##include <assert.h>
    int V = 0;
end

interface Global with
    event void e;
end

class Tx with
do
    emit global:e; // TODO: must also check if org trail is active
    _assert(0);
    _V = 1;
    _assert(0);
end

event void e;

par/or do
    await global:e;
    _V = 2;
with
    await OS_START;
    do
        var Tx t;
        _assert(0);
        await FOREVER;
    end
end
escape _V;
]],
    run = 2;
}

Test { [[
input void OS_START;

native _X,_V, _assert;
native do
    ##include <assert.h>
    int V = 0;
    int X = 0;
end

interface Global with
    event void e;
end

class Tx with
do
    _assert(_X==0); // second Tx does not execute
    _X = _X + 1;
    emit global:e;
    _assert(0);
    _V = 1;
    _assert(0);
end

event void e;

par/or do
    await global:e;
    _V = 2;
with
    await OS_START;
    do
        vector[2] Tx t;
        _assert(0);
        await FOREVER;
    end
end
escape _V+_X;
]],
    run = 3;
}

Test { [[
input void OS_START;

native _V, _assert;
native do
    ##include <assert.h>
    int V = 0;
end

class Tx with
    var int x;
    event void ok;
do
    await OS_START;
    emit  ok;
    _assert(0);
end

var int ret=1;
do
    var Tx t with
        this.x = 10;
    end;

    await t.ok;
    ret = t.x;
end
escape ret;
]],
    run = 10;
}

Test { [[
input void OS_START;

native _V, _assert;
native do
    ##include <assert.h>
    int V = 0;
end

class Tx with
    var int x=0;
    event void ok;
do
    await OS_START;
    emit  ok;
    _assert(0);
end

class U with
    var int x=0;
    event void ok;
do
    await OS_START;
    _assert(0);
    emit  ok;
end

var int ret=0;
do
    var Tx t with
        this.x = 10;
    end;
    var Tx u;
    await t.ok;
    ret = t.x;
end
escape ret;
]],
    run = 10;
}

Test { [[
class Tx with
    var int&& a1=null;
do
    var int&& a2=null;
    a1 = a2;
end
escape 10;
]],
    run = 10,
}

Test { [[
native/pure _UI_align;
class Tx with
    var _SDL_rect rect;
do
    do
        native/plain _SDL_Rect;
        var _SDL_Rect r=_SDL_Rect();
        r.x = _N;
    end
end
escape 1;
]],
    --fin = 'line 7 : attribution requires `finalize´',
    gcc = 'error: unknown type name ‘SDL_rect’',
}

Test { [[
native/pure _UI_align;
class Tx with
    var _SDL_rect rect;
do
    do
        native/plain _SDL_Rect;
        var _SDL_Rect r=_SDL_Rect();
        r.x = _UI_align(r.w, _UI_ALIGN_CENTER);
    end
end
escape 1;
]],
    --fin = 'line 7 : attribution requires `finalize´',
    gcc = 'error: unknown type name ‘SDL_rect’',
}

Test { [[
native/const _UI_ALIGN_CENTER;
native/pure _UI_align;
pre native do
    typedef struct {
        int x, w;
    } SDL_Rect;
    int UI_ALIGN_CENTER = 1;
    int UI_align (int a, int b) {
        escape 0;
    }
end
class Tx with
    var _SDL_Rect rect;
do
    do
        native/plain _SDL_Rect;
        var _SDL_Rect r=_SDL_Rect();
        r.x = _UI_align(r.w, _UI_ALIGN_CENTER);
    end
    rect.x = 1;
end
escape 1;
]],
    run = 1,
}

Test { [[
native/const _UI_ALIGN_CENTER;
native/pure _UI_align;
pre native do
    typedef struct {
        int x, w;
    } SDL_Rect;
    int UI_ALIGN_CENTER = 1;
    int UI_align (int a, int b, int c) {
        escape 0;
    }
end
class Tx with
    var _SDL_Rect rect;
do
    do
        native/plain _SDL_Rect;
        var _SDL_Rect r=_SDL_Rect();
            r.w = 1;
        r.x = _UI_align(this.rect.w, r.w, _UI_ALIGN_CENTER);
    end
end
escape 1;
]],
    --fin = 'line 17 : attribution requires `finalize´',
    run = 1,
}

Test { [[
native/const _UI_ALIGN_CENTER;
native/pure _UI_align;
pre native do
    typedef struct {
        int x, w;
    } SDL_Rect;
    int UI_ALIGN_CENTER = 1;
    int UI_align (int a, int b, int c) {
        escape 0;
    }
end
class Tx with
    var _SDL_Rect rect;
do
    do
        native/plain _SDL_Rect;
        var _SDL_Rect r=_SDL_Rect();
        r.x = (_UI_align(this.rect.w, r.w, _UI_ALIGN_CENTER) as int);
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
#define N 5
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
end
vector[N] Tx ts;
escape _V;
]],
    run = 5,
}

Test { [[
#define N 5
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
end
vector[N+1] Tx ts;
escape _V;
]],
    run = 6,
}

Test { [[
#define N 5
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
end
vector[N+1] Tx ts;
escape _V;
]],
    run = 6,
}

Test { [[
#define N 5
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
end
#error oi
vector[N+1] Tx ts;
escape _V;
]],
    lines = 'error oi',
}

Test { [[
input void OS_START;

class Tx with
do
    await FOREVER;
end
var Tx t;
par/and do
    await t;
with
    kill t;
end

escape 1;
]],
    run = 1,
}
Test { [[
input void OS_START;

class Tx with
do
    await OS_START;
end
var Tx t;
await t;

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var int size;
    code/instantaneous Run (var int size)=>Tx;
do
    code/instantaneous Run (var int size)=>Tx do
        this.size = size;
    end
    await 1s;
    escape this.size;
end

class U with
do
    do
        var int n = 0;
        do
            var Tx t = Tx.run(4);
            n = await t;
        end
    end
    do
        native/plain _char;
        var _char[8] v = [];
        var Tx t = Tx.run(2);
        par/or do
            await FOREVER;
        with
            var int n = await t;
native _assert;
            _assert(n == 2);
        end
    end
    escape 0;
end

do U;

escape 1;
]],
    run = { ['~>2s']=1 },
}
Test { [[
class Tx with
    var int size;
    code/instantaneous Run (var int size)=>Tx;
do
    code/instantaneous Run (var int size)=>Tx do
        this.size = size;
    end
    await 1s;
    escape this.size;
end

class U with
do
    do
        var int n = do Tx.run(4);
    end
    do
        native/plain _char;
        var _char[8] v = [];
        var Tx t = Tx.run(2);
        par/or do
            await FOREVER;
        with
            var int n = await t;
native _assert;
            _assert(n == 2);
        end
    end
    escape 0;
end

do U;

escape 1;
]],
    run = { ['~>2s']=1 },
}


Test { [[
class Tx with
    var int size;
    code/instantaneous Run (var int size)=>Tx;
do
    code/instantaneous Run (var int size)=>Tx do
        this.size = size;
    end
    await 1s;
    escape this.size;
end

class U with
do
    do
        vector[7] byte buf;
        var int n = do Tx.run(4);
    end
    do
        vector[] byte buf;
        var int n = do Tx.run(2);
native _assert;
        _assert(n == 2);
    end

    escape 0;
end

do U;

escape 1;
]],
    run = { ['~>2s']=1 },
}

-- CONSTRUCTOR

Test { [[
var int a with
    nothing;
end;
escape 0;
]],
    parser = 'line 1 : after `a´ : expected `=´ or `:=´ or `,´ or `;´',
    --env = 'line 1 : invalid type',
}

Test { [[
class Tx with
    var int a;
    var int b;
do
    b = a * 2;
end

var Tx t1, t2 with
    this.a = 10;
end;

escape t1.b;
]],
    parser = 'line 8 : after `t2´ : expected `=´ or `:=´ or `,´ or `;´',
}

Test { [[
class Tx with
    var int a;
    var int b=0;
do
    b = a * 2;
end

vector[2] Tx t with
    this.a = 10;
end;

escape t[0].b + t[1].b;
]],
    run = 40;
}

Test { [[
escape outer;
]],
    env = 'line 1 : types mismatch (`int´ <= `Main´)',
}

Test { [[
native _f;
_f(outer);
]],
    props = 'line 1 : `outer´ can only be unsed inside constructors',
}

Test { [[
interface I with
end

class U with
    var I&& i;
do
end

class Tx with
    var int ret = 0;
do
    var U u with
        this.i = &&outer;
    end;
    this.ret = u.i == &&this;
end

var Tx t;

escape t.ret;
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
    var int b=0;
do
    b = a * 2;
end

var Tx t with
    await 1s;
end;

escape t.b;
]],
    props = 'line 9 : not permitted inside a constructor',
}

Test { [[
class Tx with
    var int a;
    var int b=0;
do
    b = a * 2;
end

var Tx t with
    this.a = 10;
end;

escape t.b;
]],
    run = 20,
}

Test { [[
class Tx with
    var int v;
do end;
var Tx _ with
    this.v = 1;
end;
escape 1;
]],
    run = 1,
}

Test { [[
native do
    void* Ptr;
    void* myalloc (void) {
        escape NULL;
    }
    void myfree (void* ptr) {
    }
end
native/nohold _myfree;

class Tx with
    var int x = 10;
do
native _myalloc;
    do _PTR = _myalloc();
    finalize with
        _myfree(_PTR);
    end
end
var Tx t;
escape t.x;
]],
    fin = 'line 15 : cannot finalize a variable defined in another class',
}

Test { [[
native do
    int V;
    void* myalloc (void) {
        escape &V;
    }
    void myfree (void* ptr) {
    }
end
native/nohold _myfree;

class Tx with
    var int x = 10;
do
    var& void? ptr;
native _myalloc;
    do    ptr = &_myalloc();
    finalize with
        _myfree(&&ptr);
    end
end
var Tx t;
escape t.x;
]],
    env = 'line 18 : invalid operand to unary "&&" : option type',
}

Test { [[
native do
    int V;
    void* myalloc (void) {
        escape &V;
    }
    void myfree (void* ptr) {
    }
end
native/nohold _myfree;

class Tx with
    var int x = 10;
do
    var& void? ptr;
native _myalloc;
        do ptr = &_myalloc();
    finalize with
        _myfree(&&ptr!);
    end
end
var Tx t;
escape t.x;
]],
    run = 10,
}

-- TODO: bounded loop on finally

-->>> GLOBAL-DO-END / DO-PRE

Test { [[
var int tot = 1;                // 1

pre do
    tot = tot + 2;              // 3
end

tot = tot * 2;                  // 6

escape tot;
]],
    locs = 'line 4 : internal identifier "tot" is not declared',
}

Test { [[
pre do
    var int tot = 1;                // 1

    tot = tot + 2;              // 3
end

tot = tot * 2;                  // 6

escape tot;
]],
    run = 6
}

Test { [[
pre do
    var int tot = 1;                // 1

    tot = tot + 2;              // 3
end

class Tx with
do
    pre do
        tot = tot * 2;          // 6
        var int tot2 = 10;
    end
end

tot = tot + tot2;               // 16

pre do
    tot = tot + tot2;           // 26
end

escape tot;
]],
    run = 26,
}

Test { [[
pre do
var int tot = 1;                // 1
var int tot2;

    tot = tot + 2;              // 3
end

class Tx with
do
pre do
        tot = tot * 2;          // 6
        tot2 = 10;
    end
end

tot = tot + tot2;               // 16

pre do
    tot = tot + tot2;           // 26
end

escape tot;
]],
    run = 26
}

Test { [[
pre do
var int tot = 1;                // 1
var int tot2 = 1;                       // 1

    tot = tot + 2;              // 3
end

class Tx with
do
    class U with
    do
pre do
            tot = tot + 1;      // 4
            tot = tot + tot2;   // 5
        end
    end

pre do
        tot = tot * 2;          // 10
        tot2 = tot2+9;                  // 10
    end

    class V with
    do
pre do
            tot = tot + 5;      // 15
        end
    end
end

tot = tot + tot2;               // 30

pre do
    tot = tot + tot2;           // 25
    tot2 = tot2 / 2;                    // 5
end

tot2 = tot2 - 4;                        // 1

escape tot + tot2;              // 31
]],
    run = 31
}

--<<< GLOBAL-DO-END / DO-PRE

-->>> SPAWN

Test { [[
class Tx with do end
spawn Tx;
escape 1;
]],
    --env = 'line 2 : `spawn´ requires enclosing `do ... end´',
    run = 1,
}

Test { [[
native _V;
native do
    int V = 0;
end
class Tx with
do
    _V = 10;
end
do
    spawn U;
end
escape _V;
]],
    env = 'line 10 : undeclared type `U´',
}

Test { [[
native _V;
native do
    int V = 0;
end
class Tx with
do
    _V = 10;
end
do
    spawn Tx;
end
escape _V;
]],
    run = 10,
}

Test { [[
native _V;
native do
    int V = 0;
end
class Tx with
    var int a;
do
    _V = this.a;
end
do
    spawn Tx with
        this.a = 10;
    end;
end
escape _V;
]],
    run = 10,
}

Test { [[
class Tx with do end
do
    var u8? x = spawn Tx;
end
]],
    env = 'line 3 : types mismatch',
}

Test { [[
class Tx with do end
code/instantaneous Fff (void)=>void do
    spawn Tx;
end
escape 1;
]],
    props = 'line 3 : not permitted inside `function´',
}

Test { [[
class Tx with
    var int v=0;
do
end

input int E;

par/or do
    var int yyy=0;
    every xxx in E do
        spawn Tx with
            yyy = 1;
            xxx = 1;
        end;
    end;
with
end

escape 1;
]],
    run = 1,
}

Test { [[
event void e;

class Tx with do end;

par/or do
    every e do
        spawn Tx;
    end
with
    emit e;
end

escape 1;
]],
    _ana = {acc=1},
    run = 1, -- had stack overflow
}

Test { [[
event void x,e,f,g;
var int ret = 0;
class Tx with do end;
par/or do
    every x do
        loop i in [0 |> 1000[ do
            emit e;
        end
    end
with
    every e do
        emit f;
    end
with
    every f do
        emit g;
    end
with
    every g do
        ret = ret + 1;
        spawn Tx;
    end
with
    emit x;
end
escape ret;
]],
    _ana = {acc=1},
    run = 1000, -- had stack overflow
}

Test { [[
class Groundpiece with
do
end

event void x;
event void a;
event void b;
event void c;
var int ret = 0;

par/or do
    every x do
        emit b;
        ret = 10;
    end
with
    every b do
        emit a;
    end
with
    every c do
        spawn Groundpiece;
    end
with
    emit x;
end

input void OS_START;
await OS_START;
escape ret;
]],
    _ana = {acc=true},
    run = 10,
}

--<<< SPAWN

-- MEM/MEMORY POOL

Test { [[
class Tx with
do
end
pool Tx t;
escape 1;
]],
    parser = 'line 4 : after `pool´ : expected `&´ or `[´',
    --env = 'line 4 : missing `pool´ dimension',
    --parser = 'line 4 : after `Tx´ : expected `[´',
}

Test { [[
class Org with
do
end

var int n = 5;
pool[n] Org a;

escape 1;
]],
    env = 'line 6 : dimension must be constant',
}

Test { [[
class Tx with do end
pool[] Tx ts;
var Tx t;
ts = t;
escape 1;
]],
    env = 'line 4 : types mismatch',
}

Test { [[
class Tx with
do
end
pool[] Tx t;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
end
pool[1] Tx t;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with do end
pool[0] Tx ts;
var Tx&&? ok = spawn Tx in ts;
if ok? then
    escape 0;
else
    escape 1;
end
]],
    run = 1,
}

Test { [[
class Tx with
do
end
pool[1] Tx t;
var Tx&&? ok1 = spawn Tx in t with end;
var Tx&&? ok2 = spawn Tx in t;
escape (ok1?) + (ok2?) + 1;
]],
    run = 1,
    --fin = 'line 7 : unsafe access to pointer "ok1" across `spawn´',
}

Test { [[
class Tx with
do
end
pool[1] Tx t;
var Tx&&? ok1 = spawn Tx in t with end;
var int sum = 1;
if ok1? then
    watching *(ok1!) do
        var Tx&&? ok2 = spawn Tx in t;
        sum = sum + (ok1?) + (ok2?);
    end
end
escape sum;
]],
    run = 1,
}

-- POOL ITERATORS

Test { [[
class Tx with
    var int v = 0;
do
end
var Tx ts;
loop t in ts do
end
escape 1;
]],
    --fin = 'line 14 : pointer access across `await´',
    env = 'line 6 : invalid pool',
    --run = 1,
}
Test { [[
class Tx with
    var int v = 0;
do
end
pool[1] Tx ts;
loop t in ts do
end
escape 1;
]],
    --fin = 'line 14 : pointer access across `await´',
    run = 1,
}
Test { [[
class Tx with
    var int v = 0;
do
end
pool[1] Tx ts;
var Tx&&?  ok1 = spawn Tx in ts with
                this.v = 10;
              end;
var int ok2 = 0;// spawn Tx in ts;
var int ret = 0;
loop (Tx&&)t in ts do
    ret = ret + t:v;
end
escape (ok1?) + ok2 + ret;
]],
    parser = 'line 11 : after `loop´ : expected internal identifier or `do´',
    --fin = 'line 14 : pointer access across `await´',
    --run = 1,
}
Test { [[
class Tx with
    var int v = 0;
do
end
pool[1] Tx ts;
var Tx&&?  ok1 = spawn Tx in ts with
                this.v = 10;
              end;
var int ok2 = 0;// spawn Tx in ts;
var int ret = 0;
loop t in ts do
    ret = ret + t:v;
end
escape (ok1?) + ok2 + ret + 1;
]],
    --fin = 'line 14 : pointer access across `await´',
    run = 1,
}
Test { [[
class Tx with do await FOREVER; end;
pool[] Tx ts;
event void e;
spawn Tx in ts;
var int ret = 0;
loop t in ts do
    ret = ret + 1;
    spawn Tx in ts;
end
escape ret;
]],
    props = 'line 6 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
}

Test { [[
class Tx with
do
end
pool[] Tx t;
spawn Tx in t;
escape 1;
]],
    run = 1,
}
Test { [[
class Tx with
do
end
spawn Tx;
escape 1;
]],
    run = 1,
}
Test { [[
class Tx with
do
end
pool[] Tx t;
spawn Tx in t;
spawn Tx;
escape 1;
]],
    run = 1,
}

Test { [[
native do
    int V = 0;
end
class Tx with
    var int v = 0;
do
    async do end;
end
pool[] Tx ts;
spawn Tx in ts with
    this.v = 10;
    _V = _V + 10;
end;
spawn Tx with
    this.v = 20;
    _V = _V + 20;
end;
var int ret = 0;
loop t in ts do
    ret = ret + t:v;
end
escape ret + _V;
]],
    run = 40,
}

Test { [[
native do
    int V = 0;
end
class Tx with
    var int v = 0;
do
    async do end;
end
pool[] Tx ts;
spawn Tx with
    this.v = 10;
    _V = _V + 10;
end;
spawn Tx in ts with
    this.v = 20;
    _V = _V + 20;
end;
var int ret = 0;
loop t in ts do
    ret = ret + t:v;
end
escape ret + _V;
]],
    run = 50,
}

Test { [[
class Tx with
do
    await FOREVER;
end

pool[2] Tx ts;
spawn Tx in ts;
spawn Tx in ts;

input void OS_START;
await OS_START;
escape 60;
]],
    run = 60,
}
Test { [[
class Tx with
do
    await FOREVER;
end

pool[] Tx ts;
spawn Tx in ts;
spawn Tx in ts;

escape 60;
]],
    run = 60,
}

Test { [[
class Tx with
do
    await FOREVER;
end

pool[] Tx ts;
spawn Tx in ts;
spawn Tx in ts;

input void OS_START;
await OS_START;
escape 60;
]],
    run = 60,
}

Test { [[
native do
    int V = 0;
end
class Tx with
    var int v = 0;
do
    async do end;
end
pool[] Tx ts;
spawn Tx in ts with
    this.v = 10;
    _V = _V + 10;
end;
spawn Tx in ts with
    this.v = 20;
    _V = _V + 20;
end;
var int ret = 0;
loop t in ts do
    ret = ret + t:v;
end
escape ret + _V;
]],
    run = 60,
}

-->> POOL/SPAWN/OPTION

Test { [[
class Tx with do end
var Tx&& ok = spawn Tx;
escape ok != null;
]],
    env = 'line 2 : must assign to option pointer',
    --run = 1,
}

Test { [[
class U with do end
class Tx with do end
var U&&? ok = spawn Tx;
escape ok != null;
]],
    env = 'line 3 : types mismatch (`U&&?´ <= `Tx&&´)',
    --run = 1,
}

Test { [[
class Tx with do end
var Tx&&? ok = spawn Tx;
escape &&(ok!) != null;
]],
    asr = '3] runtime error: invalid tag',
    --run = 1,
}

Test { [[
class Body with do end;
var Body&&? tail = spawn Body;
await *(tail!);
escape 1;
]],
    asr = '3] runtime error: invalid tag',
    run = 1,
}

Test { [[
class Tx with do
end
var Tx&&? kkk;
pool[] Tx ppp;
spawn Tx in ppp;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with do end
var bool ok_=true;
do
    var Tx&&? ok;
    ok = spawn Tx;
    ok_ = (ok?);
end
escape ok_+1;
]],
    run = 1,
}

Test { [[
class Tx with do end
var Tx&&? ok;
var bool ok_=true;
do
    ok = spawn Tx;
    ok_ = (ok?);
end
escape ok_+1;
]],
    run = 1,
}

Test { [[
class Tx with do end
var Tx&&? ok;
do
    ok = spawn Tx;
end
escape ok?+1;
]],
    --fin = 'line 6 : pointer access across `await´',
    run = 1,
}

Test { [[
class Tx with do
    await FOREVER;
end
var Tx&&? ok;
native _assert;
do
    loop i in [0 |> 5[ do
        ok = spawn Tx;
    end
end
escape ok?+1;
]],
    --loop = 1,
    --fin = 'line 11 : pointer access across `await´',
    run = 2,
}
Test { [[
class Tx with do
    await FOREVER;
end
var Tx&&? ok;
var bool ok_=false;
native _assert;
do
    loop i in [0 |> 5[ do
        ok = spawn Tx;
        ok_ = (ok?);
    end
end
escape ok_+1;
]],
    --loop = 1,
    run = 2,
}

Test { [[
class Tx with do
    await FOREVER;
end
var Tx&&? ok;
var bool ok_=true;
native _assert;
do
    loop i in [0 |> 100[ do
        ok = spawn Tx;
    end
    var Tx&&? ok1 = spawn Tx;
    ok_ = (ok1?);
end
escape ok_+1;
]],
    --loop = 1,
    run = 1,
}
Test { [[
class Tx with do
    await FOREVER;
end
var Tx&&? ok;
native _assert;
do
    loop i in [0 |> 100[ do
        ok = spawn Tx;
    end
    ok = spawn Tx;
end
escape (ok?)+1;
]],
    --loop = 1,
    --fin = 'line 10 : pointer access across `await´',
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
end
var Tx&&? t = spawn Tx;
escape t!:a;
]],
    asr = '7] runtime error: invalid tag',
    --run = 1,
}

Test { [[
input void OS_START;
class Tx with
    var int a=0;
do
    this.a = 1;
end
var Tx&&? t = spawn Tx;
await OS_START;
escape t!:a;
]],
    --fin = 'line 9 : unsafe access to pointer "t" across `await´',
    run = '9] runtime error: invalid tag',
}

Test { [[
class Tx with do end
do
    var Tx&&? t;
    t = spawn Tx;
end
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with do end
var Tx&&? t;
t = spawn Tx;
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with
do
    par/or do
        await 10s;
    with
        await 10s;
    with
        await 10s;
    end
end
var Tx&&? t;
    t = spawn Tx;
    t = spawn Tx;
    t = spawn Tx;
escape 10;
]],
    run = 10;
}

Test { [[
class Tx with
do
    par/or do
        await 10s;
    with
        await 10s;
    with
        await 10s;
    end
end
var Tx&&? t;
do
    t = spawn Tx;
    t = spawn Tx;
    t = spawn Tx;
end
escape 10;
]],
    --fin = 'line 13 : invalid block for awoken pointer "t"',
    run = 10,
}

Test { [[
spawn i;
]],
    parser = 'line 1 : after `spawn´ : expected abstraction identifier or `do´',
}
Test { [[
_f(spawn Tx);
]],
    --parser = 'line 1 : after `(´ : expected `)´',
    parser = 'line 1 : after `(´ : expected expression',
}

Test { [[
class Tx with do end
var Tx&&? a;
a = spawn U;
]],
    env = 'line 3 : undeclared type `U´',
}

Test { [[
class Tx with do end
do
    var Tx&&? t;
    t = spawn Tx;
end
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with
do
end

var Tx&&? t = spawn Tx;
if not t? then
    escape 10;
end

escape 1;
]],
    run = 10,
}

Test { [[
class Tx with
do
    await FOREVER;
end

var Tx&&? t = spawn Tx;
if not t? then
    escape 10;
end

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
end

var Tx&&? t = spawn Tx;
await *(t!);
escape 1;
]],
    asr = 'runtime error: invalid tag',
}

Test { [[
input void OS_START;

class Tx with
do
    await OS_START;
end
pool[] Tx ts;

var Tx&&? t = spawn Tx;
await *(t!);
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;

class Tx with
do
    await FOREVER;
end

var Tx t;
par/or do
    await t;
with
    kill t;
native _assert;
    _assert(0);
end

escape 1;
]],
    run = 1,
}

-- fails w/o setjmp on parent orgs traversal
Test { [[
input void OS_START;

class Tx with
do
    await OS_START;
end

do
    var Tx t;
    await t;
end
do
native _char;
    var _char[1000] v = [];
    native/nohold _memset;
    _memset(&&v, 0, 1000);
end

escape 1;
]],
    run = 1,
}

Test { [[
input void A;

class Tx with
do
    await A;
end

pool[] Tx ts;

var Tx&&? t1 = spawn Tx;
await *(t1!);
var Tx&&? t2 = spawn Tx;
await *(t2!);

escape 1;
]],
    run = { ['~>A;~>A']=1 },
}
Test { [[
input void OS_START;

class Tx with
do
    await 1us;
end

pool[] Tx ts;

var Tx&&? t1 = spawn Tx;
await *(t1!);
var Tx&&? t2 = spawn Tx;
await *(t2!);

escape 1;
]],
    run = { ['~>2us']=1 },
}

Test { [[
input void OS_START;

class Tx with
do
    await 1us;
end
pool[] Tx ts;

var Tx&&? t1 = spawn Tx;
var Tx&&? t2 = spawn Tx;
await *(t2!);
var Tx&&? t3 = spawn Tx;
await *(t3!);

escape 1;
]],
    run = { ['~>2us']=1 },
}

-- group of tests fails w/o sudden death check while traversing children
Test { [[
input void OS_START;
native do
    int V = 0;
end

class Tx with
    event void e;
do
    await FOREVER;
end

class U with
    var& Tx t;
    var bool only_await;
do
    par/or do
        await t.e;
        _V = _V + 1;
    with
        if only_await then
            await FOREVER;
        end
        await OS_START;
        emit t.e;
    with
        await OS_START;
    end
end

var Tx t;

var U _ with
    this.t = &t;
    this.only_await = true;
end;
var U _ with
    this.t = &t;
    this.only_await = false;
end;

await OS_START;

escape _V;
]],
    run = 1,
}
Test { [[
input void OS_START;
native do
    int V = 0;
end

class Tx with
    event void e;
do
    await FOREVER;
end

class U with
    var& Tx t;
    var bool only_await;
do
    par/or do
        await t.e;
        _V = _V + 1;
    with
        if only_await then
            await FOREVER;
        end
        await OS_START;
        emit t.e;
    with
        if only_await then
            await FOREVER;
        end
        await OS_START;
    end
end

var Tx t;

var U _ with
    this.t = &t;
    this.only_await = true;
end;
var U _ with
    this.t = &t;
    this.only_await = false;
end;

await OS_START;

escape _V;
]],
    run = 2,
}
Test { [[
input void OS_START;
native do
    int V = 0;
end

class Tx with
    event void e;
do
    await FOREVER;
end

class U with
    var& Tx t;
    var bool only_await;
do
    par/or do
        await t.e;
        _V = _V + 1;
    with
        if only_await then
            await FOREVER;
        end
        await OS_START;
        emit t.e;
    with
        await OS_START;
    end
end

var Tx t;

var U _ with
    this.t = &t;
    this.only_await = false;
end;
var U _ with
    this.t = &t;
    this.only_await = true;
end;

await OS_START;

escape _V;
]],
    run = 1,
}
Test { [[
input void OS_START;
native do
    int V = 0;
end

class Tx with
    event void e;
do
    await FOREVER;
end

class U with
    var& Tx t;
    var bool only_await;
do
    par/or do
        await t.e;
        _V = _V + 1;
    with
        if only_await then
            await FOREVER;
        end
        await OS_START;
        emit t.e;
    with
        await OS_START;
    end
end

var Tx tt;

spawn U with
    this.t = &tt;
    this.only_await = false;
end;
spawn U with
    this.t = &tt;
    this.only_await = true;
end;

await OS_START;

escape _V;
]],
    run = 1,
}

-- u1 doesn't die, kills u2, which becomes dangling
Test { [[
input void OS_START;
native do
    int V = 0;
end

class Tx with
    event void e;
do
    await FOREVER;
end

class U with
    var& Tx t;
    var bool only_await;
do
    if only_await then
        await t.e;
        _V = 1;
    else
        await OS_START;
        emit t.e;
        await FOREVER;
    end
end

var Tx t;

var U _ with
    this.t = &t;
    this.only_await = false;
end;
var U _ with
    this.t = &t;
    this.only_await = true;
end;

await OS_START;

escape _V;
]],
    run = 1,
}

Test { [[
input void OS_START;
native do
    int V = 0;
end

class Tx with
    event void e;
do
    await FOREVER;
end

class U with
    var& Tx t;
    var bool only_await;
do
    if only_await then
        await t.e;
        _V = 1;
    else
        await OS_START;
        emit t.e;
        await FOREVER;
    end
end

var Tx t;

spawn U with
    this.t = &t;
    this.only_await = false;
end;
spawn U with
    this.t = &t;
    this.only_await = true;
end;

await OS_START;

escape _V;
]],
    run = 1,
}

-- fails w/o ceu_sys_stack_clear_org
Test { [[
input void OS_START;

class U with
do
    await 1us;
end

class Tx with
do
    do U;
end

do
    pool[] Tx ts;
    spawn Tx in ts;
    var Tx&&? t = spawn Tx in ts;
    await *t!;
end

escape 1;
]],
    run = { ['~>1us']=1 },
}

Test { [[
input void OS_START;

class U with
do
    await 1us;
end

class Tx with
do
    do U;
end

do
    pool[] Tx ts;
    spawn Tx in ts;
    var Tx&&? t = spawn Tx in ts;
    await *t!;
end

escape 1;
]],
    run = { ['~>1us']=1 },
}

Test { [[
input void OS_START;

class U with
do
    await 1us;
end

class Tx with
do
    do U;
end
pool[] Tx ts;

var Tx&&? t1 = spawn Tx;
var Tx&&? t2 = spawn Tx;
await *t2!;
var Tx&&? t3 = spawn Tx;
await *t3!;

escape 1;
]],
    run = { ['~>2us']=1 },
}

Test { [[
pool[0] Tx ts;
class Tx with
    var int a;
do
    this.a = 1;
end
var Tx&&? t = spawn Tx in ts;
escape not t?;
]],
    env = 'line 1 : undeclared type `Tx´',
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
end
pool[0] Tx ts;
var Tx&&? t = spawn Tx in ts;
escape not t?;
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&&? a = spawn Tx in ts;
var int sum = 0;
watching *(a!) do
    var Tx&&? b = spawn Tx in ts;
    sum = a? and (not b?);
end
escape sum;
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&&? a = spawn Tx in ts;
var int sum = 0;
watching *(a!) do
    var Tx&&? b = spawn Tx in ts;
    sum = a? and (not b?);
end
escape sum;
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
do
pool[0] Tx ts;
var Tx&&? t = spawn Tx in ts;
escape not t?;
end
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx aas;
pool[0] Tx bs;
var Tx&&? a = spawn Tx in aas;
var int sum = 0;
if a? then
    watching *a! do
        var Tx&&? b = spawn Tx in bs;
        sum = a? and (not b?);
    end
end
escape sum;
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&&? a = spawn Tx in ts;
//free(a);
var int sum = 0;
if a? then
    watching *a! do
        var Tx&&? b = spawn Tx in ts;   // fails (a is freed on end)
        sum = a? and (not b?);
    end
end
escape sum;
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&& a = null;
do
    var Tx&&? aa = spawn Tx in ts;
        a = aa!;
end
var int sum = 0;
if a != null then
    watching *a do
        var Tx&&? b = spawn Tx in ts;   // fails (a is free on end)
        sum = a!=null and (not b?) and a!=b!;
    end
end
escape sum;
]],
    fin = 'line 14 : unsafe access to pointer "a" across `spawn´ (tests.lua : 10)',
    --fin = 'line 15 : pointer access across `await´',
    --asr = ':15] runtime error: invalid tag',
    --run = 1,
}
Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[2] Tx ts;
var Tx&& a = null;
do
    var Tx&&? aa = spawn Tx in ts;
        a = aa!;
end
watching *a do
end
escape 0;
]],
    fin = 'line 13 : unsafe access to pointer "a" across `spawn´ (tests.lua : 10)',
    --fin = 'line 15 : pointer access across `await´',
    --run = 1,
}
Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[2] Tx ts;
var Tx&& a = null;
do
    var Tx&&? aa = spawn Tx in ts;
        a = aa!;
end
var int sum = 0;
if a != null then
    watching *a do
        var Tx&&? b = spawn Tx in ts;   // fails (a is free on end)
        sum = a!=null and (b?) and a!=b!;
    end
end
escape sum;
]],
    fin = 'line 14 : unsafe access to pointer "a" across `spawn´ (tests.lua : 10)',
    --fin = 'line 15 : pointer access across `await´',
    --run = 1,
}
Test { [[
class Tx with
do
end
do
    var Tx&&? aa = spawn Tx;
end
par/or do
with
end
escape 1;
]],
    --fin = 'line 15 : pointer access across `await´',
    run = 1,
}
Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&&? a;
do
    var Tx&&? aa = spawn Tx in ts;
        a = aa;
end
var int sum = 0;
if a? then
    watching *a! do
        var Tx&&? b = spawn Tx in ts;   // fails (a is free on end)
        sum = a? and (not b?);// and a! !=b;
    end
end
escape sum;
]],
    --fin = 'line 15 : pointer access across `await´',
    run = 1,
}
Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&& a=null;
do
    var Tx&&? aa = spawn Tx in ts;
        a = aa!;
end
var Tx&&? b = spawn Tx in ts;   // fails (a is free on end)
escape (not b?);
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&& a=null, b=null;
var int sum = 0;
do
    do
        var Tx&&? aa = spawn Tx in ts;
            a = aa!;
    end
    sum = a!=null;
    var Tx&&? bb = spawn Tx in ts;  // fails
        b = bb!;
end
if b != null then
    watching *b do
        var Tx&&? c = spawn Tx in ts;       // fails
        sum = (b==null) and (not c?);// and a!=b and b==c;
    end
end
escape sum;
]],
    fin = 'line 15 : unsafe access to pointer "a" across `spawn´ (tests.lua : 12)',
    --asr = ':14] runtime error: invalid tag',
    --fin = 'line 19 : pointer access across `await´',
    --run = 1,
}
Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&&? a, b;
var int sum = 0;
do
    do
        var Tx&&? aa = spawn Tx in ts;
            a = aa!;
    end
    sum = a?;
    var Tx&&? bb = spawn Tx in ts;  // fails
        b = bb;
    sum = sum and (not b?);
end
var Tx&&? c = spawn Tx in ts;       // fails
escape sum and (not c?);
]],
    --fin = 'line 19 : pointer access across `await´',
    run = 1,
}
Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&&? a, b;
var bool b_=true;
do
    do
        var Tx&&? aa = spawn Tx in ts;
            a = aa!;
    end
    var Tx&&? bb = spawn Tx in ts;  // fails
    b_ = (bb?);
end
var Tx&&? c = spawn Tx in ts;       // fails
//native/nohold _fprintf(), _stderr;
        //_fprintf(_stderr, "%p %p\n",a, b);
escape b_==false and (not c?);
]],
    run = 1,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&&? a;
var int sum = 0;
do
    var Tx&&? aa = spawn Tx in ts;
        a = aa!;
    sum = a?;
end
var Tx&&? b = spawn Tx in ts;   // fails
escape sum and (not b?);
]],
    --fin = 'line 13 : pointer access across `await´',
    run = 1,
}
Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
pool[1] Tx ts;
var Tx&& a=null;
do
    var Tx&&? aa = spawn Tx in ts;
        a = aa!;
end
var Tx&&? b = spawn Tx in ts;   // fails
escape (not b?);
]],
    run = 1,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
    await FOREVER;
end
pool[1] Tx ts;
do
    loop i in [0 |> 2[ do
        spawn Tx in ts;
    end
    loop i in [0 |> 2[ do
        spawn Tx;
    end
end
escape _V;
]],
    run = 3,
}
Test { [[
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
    await FOREVER;
end
pool[1] Tx ts;
do
    loop i in [0 |> 2[ do
        spawn Tx in ts;
    end
    loop i in [0 |> 2[ do
        spawn Tx;
    end
end
escape _V;
]],
    run = 3,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
end
do
    pool[1] Tx ts;
    loop i in [0 |> 1000[ do
        var Tx&&? ok = spawn Tx in ts;  // 999 fails
        if (not ok?) then
            escape 0;
        end
    end
end
escape _V;
]],
    --loop = 1,
    --run = 1000,
    run = 0;
}
Test { [[
input void A;
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
    await A;
end
pool[1] Tx ts;
do
    loop i in [0 |> 10[ do
        spawn Tx in ts;
    end
end
escape _V;
]],
    --loop = 1,
    run = { ['~>A']=1 },
}
Test { [[
input void A;
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
    await A;
end
pool[1] Tx ts;
do
    loop i in [0 |> 1000[ do
        var Tx&&? ok = spawn Tx in ts;
        if not ok? then
            escape 10;
        end
    end
end
escape _V;
]],
    --loop = 1,
    run = { ['~>A']=10 },
}

Test { [[
class U with
    var int v=0;
do
end

pool[10] U us;

spawn U in us;

escape 1;
]],
    run = 1,
}
Test { [[
interface I with
    var int v;
end

class U with
    var int v=0;
do
end

pool[10] I iss;

spawn U in iss;

escape 1;
]],
    run = 1,
}
Test { [[
interface I with
    var int v;
end

class Tx with
    var int u=0,v=0,x=0;
do
end

class U with
    var int v=0;
do
end

pool[10] I iss;

spawn Tx in iss;
spawn U in iss;

escape sizeof(CEU_T) >= sizeof(CEU_U);
]],
    run = 1,
}
Test { [[
class Tx with
    var int v=0;
do
end

class U with
do
end

pool[1] Tx ts;

spawn U in ts;

escape 1;
]],
    env = 'line 12 : invalid `spawn´ : types mismatch (`Tx´ <= `U´)',
}
Test { [[
interface I with
    var int v;
end

class V with
do
end

pool[1] I iss;

spawn V in iss;

escape 1;
]],
    env = 'line 11 : invalid `spawn´ : types mismatch (`I´ <= `V´)',
}
Test { [[
interface I with
    var int v;
end

class Tx with
do
end

class U with
    var int v=0;
do
end

class V with
do
    pool[1] I iss;
end

pool[1] I iss;

spawn Tx in iss;
spawn U in iss;
spawn V in iss;

escape 1;
]],
    env = 'line 21 : invalid `spawn´ : types mismatch (`I´ <= `Tx´)',
}
Test { [[
interface I with
    var int v;
end

class Tx with
do
end

class U with
    var int v=0;
do
end

class V with
do
    pool[1] I iss;
end

pool[1] I iss;

spawn U in iss;
spawn V in iss;

escape 1;
]],
    env = 'line 22 : invalid `spawn´ : types mismatch (`I´ <= `V´)',
}
Test { [[
interface I with
    var int v;
end

class Tx with do end

pool[1] I iss;

spawn Tx in iss;

escape 1;
]],
    env = 'line 9 : invalid `spawn´ : types mismatch (`I´ <= `Tx´)',
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var int u=0,v=0,x=0;
do
end

class U with
    var int v=0;
do
end

class V with
do
    pool[10] I iss;
    spawn Tx in iss;
    spawn U in iss;
end

pool[10] I iss;

spawn Tx in iss;
spawn U in iss;
spawn V in iss;

escape sizeof(CEU_T) >= sizeof(CEU_U);
]],
    env = 'line 26 : invalid `spawn´ : types mismatch (`I´ <= `V´)',
}
Test { [[
interface I with
    var int v;
end

class Tx with
    var int u=0,v=0,x=0;
do
end

class U with
    var int v=0;
do
end

class V with
do
    pool[10] I iss;
    spawn Tx in iss;
    spawn U in iss;
end

pool[10] I iss;

spawn Tx in iss;
spawn U in iss;
spawn V;

escape sizeof(CEU_T) >= sizeof(CEU_U);
]],
    run = 1,
}
Test { [[
class Tx with
    var int a;
    var int b=0;
do
    b = a * 2;
    await FOREVER;
end

var Tx&&? t =
    spawn Tx with
        this.a = 10;
    end;

escape t!:b;
]],
    run = 20,
}

-- fails w/o RET_DEAD check after ceu_app_go for PAR
Test { [[
input void OS_START;
native do
    tceu_trl* V;
end
class Tx with
do
    _V = &&__ceu_org:trls[1];
    await OS_START;
    par/or do
    with
native _assert;
        _assert(0);
    end
end
do
    var Tx t;
    await t;
end
//_V:lbl = _CEU_LBL__STACKED;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
    par/or do
    with
native _assert;
        _assert(0);
    end
end
spawn Tx;
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
do
    await OS_START;
end
spawn Tx;
await OS_START;
escape 1;
]],
    run = 1,
}

Test { [[
native _V;
native do
    int V=0;
end
input void OS_START;
class Tx with
do
    par/or do
    with
    end
    _V = _V + 1;
    await OS_START;
    _V = _V + 1;
end
var Tx&&? t1 = spawn Tx;
var Tx&&? t2 = spawn Tx;
await OS_START;
escape _V;
]],
    --run = 2,  -- blk before org
    run = 4,    -- org before blk
}

Test { [[
interface IPingu with
end

class WalkerAction with
    var& IPingu pingu;
do
end

class Pingu with
    interface IPingu;
do
    every 10s do
        spawn WalkerAction with
            this.pingu = &outer;
        end;
    end
end

escape 1;
]],
    run = 1,
}

Test { [[
interface IPingu with
end

class WalkerAction with
    var& IPingu pingu;
do
end

class Pingu with
    interface IPingu;
do
    do
        pool[] WalkerAction was;
        every 10s do
            spawn WalkerAction in was with
                this.pingu = &outer;
            end;
        end
    end
end

escape 1;
]],
    run = 1,
}

-- FREE

Test { [[
class Tx with do end
var Tx&& a = null;
//free a;
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with do end
var Tx&&? a = spawn Tx;
//free a;
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with do end
var Tx&&? a = spawn Tx;
//free a;
var Tx&&? b = spawn Tx;
//free b;
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with do end
var Tx&&? a = spawn Tx;
var Tx&&? b = spawn Tx;
//free a;
//free b;
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with do end
var Tx&&? a = spawn Tx;
var Tx&&? b = spawn Tx;
//free b;
//free a;
escape 10;
]],
    run = 10,
}

Test { [[
native _V;
native do
    int V = 0;
end
class Tx with
do
    do finalize with
        _V = _V + 1;
    end
end

var Tx&&? a = spawn Tx;
//free a;
escape _V;
]],
    run = 1,
}

Test { [[
native _V;
native do
    int V = 0;
end
class Tx with
do
    do finalize with
        _V = _V + 1;
    end
end

var Tx&&? a = spawn Tx;
var Tx&&? b = spawn Tx;
//free b;
//free a;
escape _V;
]],
    run = 2,
}

-- TODO: tests for `free´:
-- remove from tracks
-- invalid pointers
Test { [[
class Tx with do end
var Tx a;
//free a;
escape 0;
]],
    todo = 'removed free',
    env = 'line 3 : invalid `free´',
}

Test { [[
class Tx with
do
    spawn U;
end
class U with
do
    spawn Tx;
end
var Tx t;
escape 1;
]],
    env = 'line 3 : undeclared type `U´',
}

Test { [[
class Tx with do end;
class U with
    pool&[] Tx ts;
do
end
pool[] Tx ts1;
pool[2] Tx ts2;
var U _ with
    this.ts = &ts1;
end;
var U _ with
    this.ts = &ts2;
end;
escape 1;
]],
    run = 1,
}
Test { [[
native do
    int V = 0;
end
var int i=0;
var& int r = &i;

class Tx with
do
    _V = _V + 1;
    await FOREVER;
end;

pool[2] Tx ts;

class U with
    pool&[] Tx xxx;  // TODO: test also Tx[K<2], Tx[K>2]
                    //       should <= be allowed?
do
    spawn Tx in xxx;
    spawn Tx in xxx;
    spawn Tx in xxx;
    _V = _V + 10;
end

spawn Tx in ts;
var U u with
    this.xxx = &outer.ts;
end;

escape _V;
]],
    run = 12,
}

Test { [[
class Body with
    var& int sum;
do
    sum = sum + 1;
end

var int sum = 0;
var Body b with
    this.sum = &sum;
end;
sum = 10;

escape b.sum;
]],
    run = 10,
}

Test { [[
class X with do
end;

class Body with
    pool&[]  X bodies;
    var&   int    sum;
    event int     ok;
do
    var X&&? nested =
        spawn X in bodies with
        end;
    sum = sum + 1;
    emit this.ok => 1;
end

pool[1] X bodies;
var  int  sum = 1;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    run = 2,
}
Test { [[
class X with do
end;

class Body with
    pool&[]  X bodies;
    var&   int    sum;
    event int     ok;
do
    var X&&? nested =
        spawn X in bodies with
        end;
    sum = sum + 1;
    emit this.ok => 1;
end

pool[1] X bodies;
var  int  sum = 1;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

class Tx with do end;
spawn Tx;

escape sum;
]],
    run = 2,
}

Test { [[
class X with do
end;

native do
    ##ifdef CEU_ORGS_NEWS_POOL
    ##error bug found
    ##endif
end

class Body with
    pool&[]  X bodies;
    var&   int    sum;
    event int     ok;
do
    var X&&? nested =
        spawn X in bodies with
        end;
    sum = sum + 1;
    emit this.ok => 1;
end

pool[] X bodies;
var  int  sum = 1;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

class Tx with do end;
spawn Tx;

escape sum;
]],
    run = 2,
}

Test { [[
class Job with
    var u8 id = 0;
    event void nextRequest;
do
    // do job
    await 1s;
end

pool[10] Job jobs;
pool&[10] Job jobs_alias = &jobs;

var Job&& ptr = null;
loop j in jobs do
    ptr = j;
    if true then break; end
end // ok, no compile error

ptr = null;
loop j in jobs_alias do
    ptr = j;
    if true then break; end
end // compile error occurs

escape 1;
]],
    run = 1,
}

-- problems w/o ceu_sys_stack_clear_org
Test { [[
input void OS_START;

class Tx with
do
    await 1us;
end

do
    var Tx t;
    await t;
end
do
native _char;
    var _char[1000] v = [];
    native/nohold _memset;
    _memset(&&v, 0, 1000);
end

escape 1;
]],
    run = { ['~>2us']=1 },
}

Test { [[
input void OS_START;

class U with
do
    await 1us;
end

class Tx with
do
    do U;
end

do
    var Tx t1;
    var Tx t2;
    await t2;
end
do
native _char;
    var _char[1000] v = [];
    native/nohold _memset;
    _memset(&&v, 0, 1000);
    var Tx t3;
    await t3;
end

escape 1;
]],
    run = { ['~>2us']=1 },
}

Test { [[
class U with
    event void ok;
do
    do finalize with
        _V = _V + 4;
    end
    await 1ms;
    emit this.ok;
    await FOREVER;
end;
class Tx with do
    do finalize with
        _V = _V + 2;
    end
    var U u;
    await FOREVER;
end;
native do
    int V = 1;
end
do finalize with
    _V = 1000;
end
do finalize with
    _V = 1000;
end
do finalize with
    _V = 1000;
end
par/or do
    await 1s;
with
    do
        var Tx t;
        var U u;
        par/or do
            await u.ok;
        with
            await u.ok;
        end;
    end
    var Tx t1;
    var U u1;
    await u1.ok;
native _assert;
    _assert(_V == 11);
end
_assert(_V == 21);
escape _V;
]],
    run = { ['~>1s']=21 },
}

-- SPAWN / RECURSIVE

Test { [[
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
    if _V < 10 then
        spawn Tx;
    end
end
var Tx t;
escape _V;
]],
    wrn = 'line 8 : unbounded recursive spawn',
    run = 10,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
    spawn Tx;
end
var Tx t;
escape _V;
]],
    wrn = 'line 7 : unbounded recursive spawn',
    run = 101,  -- tests force 100 allocations at most
    --asr = 'runtime error: stack overflow',
}
Test { [[
native do
    int V = 0;
end
class Tx with
do
    _V = _V + 1;
    spawn Tx;
    await FOREVER;
end
var Tx t;
escape _V;
]],
    wrn = 'line 7 : unbounded recursive spawn',
    run = 101,  -- tests force 100 allocations at most
}
Test { [[
class Body with
    pool&[]  Body bodies;
    var&   int    sum;
    event int     ok;
do
    var Body&&? nested =
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    if nested? then
        watching *nested! do
            await nested!:ok;
        end
    end
    sum = sum + 1;
    emit this.ok => 1;
end

pool[4] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    wrn = 'line 7 : unbounded recursive spawn',
    run = 5,
}

Test { [[
class Body with
    pool&[] Body bodies;
    var&  int     sum;
do
    var Body&&? nested =
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    if nested? then
        await *nested!;
    end
    sum = sum + 1;
end

pool[] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    wrn = 'line 6 : unbounded recursive spawn',
    run = 101,
}

--[[
-- Trying to create an infinite loop with
-- bounded pool + recursive spawn.
-- Is it possible?
--      - no awaits from start to recursive spawn
--      - with nothing else, the Nth spawn will fail
--      - from the fail, the last spawn resumes
--      - in the worst scenario, it finishes and opens a new slot
--      - if the recursive spawn tries another recursive spawn in sequence,
--          this new one will succeed, but the same resoning above holds
--          I'm just duplicating the successes, but not really unbounded yet
--      - I cannot have indirect recursion
--      - So, the only possibility is with a loop enclosing the recursive spawn
--      - But in this case, the language will warn if this loop has no awaits.
--      - It will change the message from "unbounded recursive spawn"
--          to "tight loop", which is correct!
--]]

Test { [[
class Body with
    pool&[1] Body bodies;
    var&  int     sum;
do
    var Body&&? nested =
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    if nested? then
        await *nested!;
    end
    sum = sum + 1;
end

pool[1] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    run = 2,
}
Test { [[
class Body with
    pool&[1] Body bodies;
    var&  int     sum;
do
    var Body&&? nested =
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    sum = sum + 1;
end

pool[1] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    run = 2,
}

Test { [[
class Body with
    pool&[1] Body bodies;
    var&  int     sum;
do
    var Body&&? nested =
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    sum = sum + 1;
end

pool[1] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    run = 2,
}

Test { [[
class Body with
    pool&[1] Body bodies;
    var&  int     sum;
do
    spawn Body in bodies with
        this.bodies = &bodies;
        this.sum    = &sum;
    end;
    sum = sum + 1;
    spawn Body in bodies with
        this.bodies = &bodies;
        this.sum    = &sum;
    end;
end

pool[1] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    run = 3,
}

Test { [[
class Body with
    pool&[1] Body bodies;
    var&  int     sum;
do
    sum = sum + 1;
    loop do
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    end
end

pool[1] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    tight = 'line 6 : tight loop',
}

Test { [[
class Body with
    pool&[1] Body bodies;
    var&  int     sum;
do
    sum = sum + 1;
    loop do
        var Body&&? t = spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
        watching *t! do
            await FOREVER;
        end
    end
end

pool[1] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape sum;
]],
    tight = 'line 6 : tight loop',
}

Test { [[
class Sum with
    var int&& v;
do
    await FOREVER;
end

class Body with
    pool&[]  Body bodies;
    var&   Sum    sum;
do
    *this.sum.v = *this.sum.v + 1;
    spawn Body in this.bodies with
        this.bodies = &bodies;
        this.sum    = &sum;
    end;
end

var int v = 0;
var Sum sum with
    this.v = &&v;
end;

pool[7] Body bodies;
do Body with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape v;
]],
    fin = 'line 11 : unsafe access to pointer "v" across `class´ (tests.lua : 7)',
    --wrn = true,
    --run = 8,
}
Test { [[
class Sum with
    var int&& v;
do
    await FOREVER;
end

class Body with
    pool&[]  Body bodies;
    var&   Sum    sum;
do
    await 1s;
    *this.sum.v = *this.sum.v + 1;
    spawn Body in this.bodies with
        this.bodies = &bodies;
        this.sum    = &sum;
    end;
end

var int v = 0;
var Sum sum with
    this.v = &&v;
end;

pool[7] Body bodies;
do Body with
    this.bodies = &bodies;
    this.sum    = &sum;
end;

escape v;
]],
    fin = 'line 12 : unsafe access to pointer "v" across `class´ (tests.lua : 7)',
}

    -- Await/KILL ORG

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
end
var Tx a;
var int ret = 0;
par/and do
    await a;
    ret = ret + 1;
with
    kill a;
with
    await a;
    ret = ret * 2;
end
escape ret;
]],
    _ana = { acc=3 },
    run = 2,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
end
var Tx a;
var int ret = 0;
par/and do
    watching a do
        await FOREVER;
    end
    ret = 10;
with
    kill a;
end
escape ret;
]],
    run = 10,
}

Test { [[
input void OS_START;

class Tx with
    var int a=0;
do
end

event Tx&& e;

par/or do
    await OS_START;
    var Tx a;
    emit e => &&a;
    await FOREVER;
with
    var Tx&& pa = await e;
    watching *pa do
        await FOREVER;
    end
end

escape 1;
]],
    env = 'line 8 : invalid event type',
    --env = 'line 13 : wrong argument : cannot pass pointers',
    --run = 1,
}

Test { [[
input void OS_START;

class Tx with
    var int a=0;
do
    await 1s;
end

event Tx&& e;

par do
    var Tx&& pa = await e;
    watching *pa do
        await FOREVER;
    end
    escape -1;
with
    await OS_START;
    do
        var Tx a;
        emit e => &&a;
    end
    await 2s;
    escape 1;
end
]],
    env = 'line 9 : invalid event type',
    --env = 'line 21 : wrong argument : cannot pass pointers',
    --run = { ['~>2s']=1 },
}

Test { [[
class Tx with
do
end
var Tx a;
var int&& v = await a;
escape 1;
]],
    env = 'line 5 : types mismatch (`int&&´ <= `int´)',
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
var Tx a;
var int rrr = 0;
par/and do
    var int v = await a;
    rrr = rrr + v;
with
    kill a => 10;
with
    var int v = await a;
    rrr = rrr + v;
end
escape rrr;
]],
    _ana = { acc=true },
    run = 20,
}

Test { [[
class Tx with
    var int a=0;
do
    this.a = 1;
    await FOREVER;
end
var Tx a;
var int ret = 10;
par/and do
    var int v =
    watching a do
        await FOREVER;
    end;
    ret = v;
with
    kill a => 1;
end
escape ret;
]],
    run = 1,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    do finalize with
        _V = 10;
    end
    await FOREVER;
end
do
    var Tx t;
end
escape _V;
]],
    run = 10,
}
Test { [[
native do
    int V = 0;
end
class Tx with
do
    do finalize with
        _V = 10;
    end
    await FOREVER;
end
do
    pool[] Tx ts;
    var Tx&&? t = spawn Tx in ts;
end
escape _V;
]],
    run = 10,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    do finalize with
        _V = 10;
    end
    await FOREVER;
end
var Tx&&? t = spawn Tx;
kill *t!;
escape _V;
]],
    run = 10,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    do finalize with
        _V = 10;
    end
    await FOREVER;
end
var Tx t;
kill t;
escape _V;
]],
    run = 10,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    do finalize with
        _V = 10;
    end
    await FOREVER;
end
var Tx t;
par/and do
    kill t;
with
    await t;
    _V = _V * 2;
end
escape _V;
]],
    run = 20,
}

Test { [[
class Tx with
    var int v = 10;
do
    await 1s;
end

input void OS_START;
event Tx&& e;

var int ret = 2;

par/and do
    await OS_START;
    var Tx&&? t = spawn Tx;
    emit e => t!;
    ret = ret + t!:v;
with
    var Tx&& t1 = await e;
    ret = ret * 2;
end

escape ret;
]],
    env = 'line 8 : invalid event type',
    --env = 'line 15 : wrong argument : cannot pass pointers',
    --run = { ['~>1s'] = 14 },
    --fin = 'line 16 : unsafe access to pointer "t" across `emit´',
}

Test { [[
class Tx with
    var int v = 10;
do
    await 1s;
end

input void OS_START;
event Tx&& e;

var int ret = 1;

par/and do
    await OS_START;
    var Tx&&? t = spawn Tx;
    watching *t! do
        emit e => t!;
        ret = ret + t!:v;
        await *t!;
        ret = ret + 1;
    end
with
    var Tx&& t1 = await e;
    ret = ret * 2;
end

escape ret;
]],
    env = 'line 8 : invalid event type',
    --env = 'line 16 : wrong argument : cannot pass pointers',
    --run = { ['~>1s'] = 12 },
}

Test { [[
class Tx with
    var int v = 10;
do
    await FOREVER;
end

var Tx&&? t = spawn Tx;
do finalize with
    kill *t!;
end

escape 10;
]],
    props = 'line 9 : not permitted inside `finalize´',
}

Test { [[
class Tx with
    var int v = 10;
do
    await FOREVER;
end

input void OS_START;
event Tx&& e;

var int ret = 1;

par/and do
    await OS_START;
    var Tx&&? t = spawn Tx;
    ret = ret * 2;
    watching *t! do
        emit e => t!;
        ret = ret + t!:v;
        await *t!;
        ret = -1;
    end
    ret = ret * 2;
with
    var Tx&& t1 = await e;
    ret = ret + t1:v;
    kill *t1;
    ret = ret + 1;
end

escape ret;
]],
    env = 'line 8 : invalid event type',
    --env = 'line 17 : wrong argument : cannot pass pointers',
    --run = 25,
}

Test { [[
class Tx with
do
    await FOREVER;
end
var int ret = 0;
loop i do
    var Tx t1;
    par/or do
        await t1;
    with
        kill t1;
        await FOREVER;
    end

    var Tx&&? t = spawn Tx;
    par/or do
        await *t!;
    with
        kill *t!;
        await FOREVER;
    end
    if i == 10 then
        break;
    else
        ret = ret + 1;
    end
end
escape ret;
]],
    wrn = true,
    loop = true,
    --tight = 'line 6 : tight loop',
    run = 10,
}

Test { [[
class Tx with
do
end
var int ret = 0;
loop i do
    var Tx t1;
    par/or do
        await t1;
    with
        kill t1;
        await FOREVER;
    end

    var Tx&&? t = spawn Tx;
    par/or do
        if t? then
            await *t!;
        end
    with
        kill *t!;
        await FOREVER;
    end
    if i == 10 then
        break;
    else
        ret = ret + 1;
    end
end
escape ret;
]],
    wrn = true,
    loop = true,
    --tight = 'line 6 : tight loop',
    run = 10,
}

Test { [[
class Tx with
do
    await FOREVER;
end

pool[] Tx ts;

loop t1 in ts do
    loop t2 in ts do
        kill *t1;
        kill *t2;
    end
end

escape 1;
]],
    fin = 'line 10 : unsafe access to pointer "t1" across `loop´ (tests.lua : 9)',
    --fin = 'line 11 : unsafe access to pointer "t2" across `kill´',
}

Test { [[
class Tx with
do
    await FOREVER;
end

pool[] Tx ts;

loop t1 in ts do
    loop t2 in ts do
        watching *t2 do
            kill *t1;
            kill *t2;
        end
    end
end

escape 1;
]],
    fin = ' line 11 : unsafe access to pointer "t1" across `loop´ (tests.lua : 9)',
}

Test { [[
class Tx with
do
    await FOREVER;
end

pool[] Tx ts;

loop t1 in ts do
    watching *t1 do
        loop t2 in ts do
            watching *t2 do
                kill *t1;
                kill *t2;
            end
        end
    end
end

escape 1;
]],
    props = 'line 8 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 1,
}

Test { [[
input void OS_START;
event void a;
class Tx with do end
do
    var Tx t;
    par/or do
        await a;
native _assert;
        _assert(0);
    with
        await OS_START;
    end
    emit a;
end
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
event void a;
class Tx with do
    await FOREVER;
end
do
    var Tx t;
    par/or do
        await t;
native _assert;
        _assert(0);
    with
        await OS_START;
    end
    kill t;
end
escape 1;
]],
    run = 1,
}

-- outer organism dies, nested organism has to awake block
Test { [[
native do
    int V = 1;
end

class S with
do
    await FOREVER;
end

class Tx with
    var S&& s;
do
    watching *s do
        every 1s do
            _V = _V + 1;
        end
    end
end

par/or do
    var S s;
    var Tx&&? t =
        spawn Tx with
            this.s = &&s;
        end;
    await *t!;
with
end

await 5s;

escape _V;
]],
    run = { ['~>10s']=1 },
}

Test { [[
input void OS_START;

class OrgC with
do
    await FOREVER;
end

event void signal;
var int ret = 0;

par/or do
    loop do
        watching signal do
            do OrgC;
        end
        ret = ret + 1;
    end
with
    await OS_START;
    emit signal;
end

escape ret;
]],
    wrn = true,
    loop = true,
    run = 1,
}

Test { [[
class Tx with
do
end

do Tx;

vector[31245] Tx ts;

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
end

do Tx;

vector[31246] Tx ts;

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
end

do Tx;

vector[65500] Tx ts;

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
end

do Tx;

vector[65532] Tx ts;

escape 1;
]],
    valgrind = false,   -- TODO: why?
    run = 1,
}

Test { [[
class Tx with
do
end

do Tx;

vector[65533] Tx ts;

escape 1;
]],
    run = ':4] runtime error: too many dying organisms',
}

-->>> SPAWN-DO SUGAR

Test { [[
spawn do
end
escape 1;
]],
    ana = 'line 1 : `spawn´ body must never terminate',
}
Test { [[
spawn do
    await FOREVER;
end
escape 1;
]],
    run = 1,
}

Test { [[
var int xxx=0;
spawn do
    xxx = 1;
    await FOREVER;
end
escape xxx;
]],
    _ana = {acc=1},
    run = 1,
}

Test { [[
var int x;
spawn do
    x = 1;
end
escape x;
]],
    ref = 'line 1 : uninitialized variable "x" crossing compound statement (tests.lua:2)',
    --run = 1,
}

Test { [[
var int x =
    spawn do
        escape 1;
    end;
escape x;
]],
    parser = 'line 2 : after `spawn´ : expected abstraction identifier',
}

Test { [[
input void OS_START;
var int x = 1;
spawn do
    every 1s do
        x = 1;
    end
end
await OS_START;
escape x;
]],
    run = 1,
}

Test { [[
input void OS_START;
var int x = 1;
spawn do
    loop do
        await 1s;
        if 1 == 1 then
            break;
        end
    end
end
await OS_START;
escape x;
]],
    ana = 'line 3 : `spawn´ body must never terminate',
}

Test { [[
input void OS_START;
var int x = 0;
spawn do
    await OS_START;
    x = 1;
end
await OS_START;
escape x;
]],
    ana = 'line 3 : `spawn´ body must never terminate',
}

Test { [[
input void OS_START;
var int x = 0;
spawn do
    await OS_START;
    x = 1;
    await FOREVER;
end
await OS_START;
escape x;
]],
    _ana = {acc=1},
    run = 1,
}

Test { [[
var int xxx=0;
spawn do
    this.xxx = 1;
    await FOREVER;
end
escape xxx;
]],
    _ana = {acc=1},
    run = 1,
}

Test { [[
class Tx with
    var int xxx=0;
do
    var int aaa = 0;
    spawn do
        this.aaa = 10;
        this.xxx = this.aaa;
        await FOREVER;
    end
    escape aaa;
end
var int ret = do Tx;
escape ret;
]],
    _ana = {acc=1},
    run = 10,
}

--<<< SPAWN-DO SUGAR

-->>> DO-Tx SUGAR

Test { [[
do Tx;
escape 0;
]],
    parser = 'line 1 : after `Tx´ : expected `(´',
}

Test { [[
await Tx;
escape 0;
]],
    parser = 'line 1 : after `Tx´ : expected `(´',
}

Test { [[
await Tx();
escape 0;
]],
    tops = 'line 1 : abstraction "Tx" is not declared',
}

Test { [[
class Tx with
do
end
do Tx;
escape 0;
]],
    run = 0,
    --locs = 'line 4 : internal identifier "ok" is not declared',
}

Test { [[
class Tx with
    event void ok;
do
    emit ok;
end
par/or do
    loop do
        do Tx;
    end
with
end
escape 1;
]],
    tight = 'line 7 : tight loop',
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
    event void ok;
do
    emit ok;
end
par do
    do Tx;
    escape 1;
with
    await OS_START;
    escape 2;
end
]],
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
    event void ok;
do
    await OS_START;
    emit ok;
end
do Tx;
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
    event int ok;
do
    await OS_START;
    emit ok => 1;
end
do Tx;
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
do
    await OS_START;
    escape 1;
end
var int ddd = do Tx;
escape ddd;
]],
    run = 1,
}
Test { [[
class Tx with
do
    escape 1;
end
var int ddd = do Tx;
escape ddd;
]],
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
    var int v;
do
    await OS_START;
    escape v;
end
var int v = do Tx with
    this.v = 10;
end;
escape v;
]],
    run = 10,
}

Test { [[
input void OS_START;
class Tx with
    var int vv;
do
    await OS_START;
    escape vv;
end
var int a;
a = do Tx with
    this.vv = 10;
end;
escape a;
]],
    run = 10,
}

Test { [[
input void OS_START;
class Tx with
    var int v;
do
    await OS_START;
    escape 10;
end
var int&& v = do Tx with
    this.v = 10;
end;
escape *v;
]],
    env = 'line 8 : types mismatch (`int&&´ <= `int´)',
}

Test { [[
input void OS_START;
class Tx with
    var int v;
do
    await OS_START;
    escape v;
end
var int v;
v = do Tx with
    this.v = 10;
end;
escape v;
]],
    run = 10,
}

Test { [[
input void OS_START;
class Tx with
    var int v;
do
    await OS_START;
    escape (v,v*2);
end
var int v1, v2;
(v1,v2) = do Tx with
    this.v = 10;
end;
escape v1+v2;
]],
    parser = 'line 6 : after `v´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `)´',
    --env = 'line 10 : arity mismatch',
    --run = 30,
}

Test { [[
input void OS_START;

class Mix with
  var int cup_top;
  event void ok;
do
    await OS_START;
    emit ok;
end

class ShuckTip with
do
    await FOREVER;
end

do
    var int dilu_start = 0;
    do
        var Mix m with
            this.cup_top = dilu_start;
        end;
        await m.ok;
    end
end
do
    var ShuckTip s;
end

escape 1;
]],
    run = 1,
}
Test { [[
input void OS_START;

class Mix with
  var int cup_top;
  event void ok;
do
  await OS_START;
  emit ok;
end

class ShuckTip with
do
end

do
    var int dilu_start = 0;
    do
        var Mix m with
            this.cup_top = dilu_start;
        end;
        await m.ok;
    end
end
do
    var ShuckTip s;
    await s;
end

escape 1;
]],
    run = 1,
}

Test { [[
input void MOVE_DONE;

class Mix with
  var int cup_top;
  event void ok;
do
  await MOVE_DONE;
  emit ok;
end

class ShuckTip with
  event void ok;
do
end

par/or do
  do
    var int dilu_start = 0;
    do
      var Mix m with
        this.cup_top = dilu_start;
      end;
      await m.ok;
    end
  end
  do ShuckTip;
with
  async do
    emit MOVE_DONE;
  end
end

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with do
    await FOREVER;
end
var Tx&&? ok;
native do ##include <assert.h> end
native _assert;
do
    loop i in [0 |> 100[ do
        ok = spawn Tx;
    end
    _assert(ok?);
    ok = spawn Tx;
    ok = spawn Tx;
    _assert(not ok?);
end
do
    loop i in [0 |> 100[ do
        ok = spawn Tx;
    end
    _assert(not ok?);
end
do
    loop i in [0 |> 101[ do
        ok = spawn Tx;
    end
    _assert(not ok?);
end
escape (not ok?);
]],
    --loop = 1,
    --fin = 'line 11 : pointer access across `await´',
    run = 1,
}

Test { [[
class Tx with do
    await FOREVER;
end
native do ##include <assert.h> end
native _assert;
do
    loop i in [0 |> 100[ do
        var Tx&&? ok;
        ok = spawn Tx;
        _assert(ok?);
    end
    var Tx&&? ok1 = spawn Tx;
    _assert(not ok1?);
    var Tx&&? ok2 = spawn Tx;
    _assert(not ok2?);
end
do
    loop i in [0 |> 100[ do
        var Tx&&? ok;
        ok = spawn Tx;
        _assert(ok?);
    end
end
do
    loop i in [0 |> 101[ do
        var Tx&&? ok;
        ok = spawn Tx;
        _assert(i<100 or (not ok?));
    end
end
escape 1;
]],
    --loop = 1,
    --run = 1,
    asr = true,
}

Test { [[
class Tx with do
    await FOREVER;
end
native do ##include <assert.h> end
native _assert;
do
    pool[] Tx ts;
    loop i in [0 |> 100[ do
        var Tx&&? ok;
        ok = spawn Tx in ts;
        _assert(not ok?);
    end
    var Tx&&? ok1 = spawn Tx;
    _assert(not ok1?);
    var Tx&&? ok2 = spawn Tx;
    _assert(not ok2?);
end
do
    pool[] Tx ts;
    loop i in [0 |> 100[ do
        var Tx&&? ok;
        ok = spawn Tx in ts;
        _assert(ok?);
    end
end
do
    pool[] Tx ts;
    loop i in [0 |> 101[ do
        var Tx&&? ok;
        ok = spawn Tx in ts;
        if i < 100 then
            _assert(ok?);
        else
            _assert(not ok?);
        end
    end
end
escape 1;
]],
    --loop = 1,
    --run = 1,
    asr = true,
}

Test { [[
native do ##include <assert.h> end
native _V;
native do
    int V = 0;
end
class Tx with
    var int inc;
do
    do finalize with
        _V = _V + this.inc;
    end
    await FOREVER;
end
var int v = 0;
do
    pool[] Tx ts;
    loop i in [0 |> 200[ do
        var Tx&&? ok =
            spawn Tx in ts with
                this.inc = 1;
            end;
        if (not ok?) then
            v = v + 1;
        end
    end

    input void OS_START;
    await OS_START;
end
native _assert;
_assert(_V==100 and v==100);
escape _V+v;
]],
    --loop = 1,
    run = 200,
}

Test { [[
do
    var int i = 1;
    every 1s do
        spawn HelloWorld(i);
        i = i + 1;
    end
end
]],
    tops = 'line 4 : abstraction "HelloWorld" is not declared',
}

Test { [[
native _V;
native do
    int V = 0;
end
class Tx with
do
    await 2s;
    _V = _V + 1;
end
do
    spawn Tx;
    await 1s;
    spawn Tx;
    await 1s;
    spawn Tx;
    await 1s;
    spawn Tx;
    await 50s;
end
escape _V;
]],
    run = { ['~>100s']=4 },
}

Test { [[
input void OS_START;
native _V;
native do
    int V = 1;
end
class Tx with
do
    await OS_START;
    _V = 10;
end
do
    spawn Tx;
    await OS_START;
end
escape _V;
]],
    --run = 1,  -- blk before org
    run = 10,   -- org before blk
}

Test { [[
input void OS_START;
native _V;
native do
    int V = 1;
end
class Tx with
do
    _V = 10;
end
do
    spawn Tx;
    await OS_START;
end
escape _V;
]],
    run = 10,
}

Test { [[
class Tx with do end;
var Tx a;
var Tx&& b;
b = &&a;
escape 1;
]],
    run = 1,
    --env = 'line 4 : invalid attribution',
}

Test { [[
class Tx with do
    await FOREVER;
end;
var Tx&&? a = spawn Tx;
var Tx&& b;
b = a!;
escape 10;
]],
    run = 10;
}

Test { [[
class Tx with do end;
var Tx&&? a = spawn Tx;
var Tx&& b;
b = a!;
escape 10;
]],
    asr = '4] runtime error: invalid tag',
}

Test { [[
class Tx with
    var int v=0;
do
    await FOREVER;
end

var Tx&& a=null;
do
    var Tx&&? b = spawn Tx;
    b!:v = 10;
    a = b!;
end
escape a:v;
]],
    --fin = 'line 10 : attribution requires `finalize´',
    --fin = 'line 12 : pointer access across `await´',
    --run = 10,
    fin = 'line 13 : unsafe access to pointer "a" across `spawn´ (tests.lua : 9)',
}
Test { [[
class Tx with
    var int v=0;
do
    await FOREVER;
end

var Tx&&? a;
do
    var Tx&&? b = spawn Tx;
    b!:v = 10;
    a = b!;
end
escape a!:v;
]],
    --fin = 'line 10 : attribution requires `finalize´',
    --fin = 'line 12 : pointer access across `await´',
    run = 10,
}
Test { [[
class Tx with
    var int v=0;
do
    await FOREVER;
end

var Tx&&? a;
do
    var Tx&&? b = spawn Tx;
    b!:v = 10;
    a = b!;
    escape a!:v;
end
]],
    --fin = 'line 10 : attribution requires `finalize´',
    run = 10,
}

Test { [[
class Tx with
    var int v=0;
do
end

var Tx&& a=null;
do
    var Tx&&? b = spawn Tx;
    b!:v = 10;
    a = b!;
end
await 1s;
escape a:v;
]],
    fin = 'line 13 : unsafe access to pointer "a" across `spawn´ (tests.lua : 8)'
}

Test { [[
class Tx with
    var int v=0;
do
    await FOREVER;
end

var Tx&& a=null;
var Tx aa;
do
    var Tx&&? b = spawn Tx;
    b!:v = 10;
        a = b!;
end
escape a:v;
]],
    fin = 'line 14 : unsafe access to pointer "a" across `spawn´ (tests.lua : 10)',
}

Test { [[
native _V;
native do
    int V = 1;
end
class Tx with
    var int v=0;
do
    do finalize with   // enters!
        _V = 10;
    end
    await FOREVER;
end

var Tx&& a=null;
var Tx aa;
do
    pool[] Tx ts;
    var Tx&&? b = spawn Tx in ts;
    b!:v = 10;
        a = b!;
end
escape _V;
]],
    run = 10,
}

Test { [[
input void OS_START;
native _V;
native do
    int V = 0;
end
class Tx with
    var int v=0;
do
    do finalize with
        _V = 10;
    end
    await FOREVER;
end

var Tx&& a=null;
var Tx aa;
do
    pool[] Tx ts;
    var Tx&&? b = spawn Tx in ts;
    b!:v = 10;
        a = b!;
    await OS_START;
end
escape _V;
]],
    run = 10,
}

Test { [[
native _V;
native do
    int V = 5;
end
class Tx with
    var int v=0;
do
    do finalize with   // enters!
        _V = 10;
    end
    await FOREVER;
end

var Tx&& a=null;
do
    pool[] Tx ts;
    var Tx&&? b = spawn Tx in ts;
    b!:v = 10;
        a = b!;
end
escape _V;
]],
    run = 10,
}
Test { [[
input void OS_START;
native _V;
native do
    int V = 5;
end
class Tx with
    var int v=0;
do
    do finalize with
        _V = 10;
    end
    await FOREVER;
end

var Tx&& a=null;
do
    pool[] Tx ts;
    var Tx&&? b = spawn Tx in ts;
    b!:v = 10;
        a = b!;
    await OS_START;
end
escape _V;
]],
    run = 10,
}
Test { [[
class Tx with
    var int&& i1=null;
do
    var int i2=0;
    i1 = &&i2;
end
var Tx a;
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with do end
var Tx&& t1=null;
do
do
    var Tx t2;
    t1 = &&t2;
end
end
escape 10;
]],
    fin = 'line 6 : attribution to pointer with greater scope',
    run = 10,
}

Test { [[
class Tx with do end
var Tx&& t1=null;
do
do
    var Tx t2;
    //finalize
        t1 = &&t2;
    //with
        //nothing;
    //end
end
end
escape 10;
]],
    fin = 'line 7 : attribution to pointer with greater scope',
    run = 10,
}

Test { [[
class Tx with do end
var Tx&&? t;
do
    t = spawn Tx;
end
escape 10;
]],
    run = 10,
    --fin = 'line 4 : invalid block for awoken pointer "t"',
}

Test { [[
class Tx with do end
var Tx&&? a = spawn Tx;
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with do end
class U with do end
var Tx&&? a;
a = spawn U;
]],
    env = 'line 4 : types mismatch',
}

Test { [[
native _V;
input void OS_START;
native do
    int V = 0;
end

class Tx with
    var int a=0;
do
    do finalize with
        _V = 1;
    end
    a = 10;
end

var int ret = 0;

do
    var Tx&&? o;
    o = spawn Tx;
    await OS_START;
    ret = o!:a;
end

escape ret + _V;
]],
    run = '22] runtime error: invalid tag',
    --run = 11,
    --fin = 'line 22 : unsafe access to pointer "o" across `await´',
}

Test { [[
input void OS_START, B;
native _V;
native do
    int V = 0;
end

class Tx with
    var int a=0;
do
    do finalize with
        _V = 1;
    end
    a = 10;
    await 1s;
end

var int ret = 0;

par/or do
    pool[] Tx ts;
    var Tx&&? o;
    o = spawn Tx in ts;
    //await OS_START;
    ret = o!:a;
with
    await B;
end

escape ret + _V;
]],
    run = { ['~>B']=11 },
}

Test { [[
class Tx with
do
    await FOREVER;
end

par/or do
    spawn Tx;
with
native _assert;
    _assert(0);
end

escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START, B;
native _V;
native do
    int V = 0;
end

class Tx with
    var int a=0;
do
    do finalize with
        _V = 1;
    end
    a = 10;
    await 1s;
end

var int ret = 0;

par/or do
    var Tx&&? o;
    o = spawn Tx;
    //await OS_START;
    ret = o!:a;
with
    await B;
end

escape ret + _V;    // V still 0
]],
    run = { ['~>B']=10 },
}

Test { [[
class V with
do
end

var V&&? v;
v = spawn V;
await 1s;

escape 10;
]],
    run = { ['~>1s']=10, }
}

Test { [[

class V with
do
end

class U with

do
    var V&&? vv = spawn V;
end

class Tx with
    var U u;
do
end

var Tx t;
escape 1;
]],
    props = 'line 13 : not permitted inside an interface',
}

Test { [[

class V with
do
end

class U with
 
do
    var V&&? vv = spawn V;
end

class Tx with
    var U&& u=null;
do
    var U uu;
    this.u = &&uu;
end

var Tx t;
escape 1;
]],
    run = 1,
}

Test { [[
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U u;
do
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

var Tx t;
escape _V;
]],
    props = 'line 26 : not permitted inside an interface',
}
Test { [[
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
    if v? then end;
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U&& u=null;
do
    var U uu;
    u = &&uu;
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

var Tx t;
escape _V;
]],
    run = 1,
}

Test { [[
input void OS_START;
native _f, _V;
native do
    int V = 1;
end

class V with
do
    do finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V v;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U u;
do
    await FOREVER;
end

do
    var Tx t;
end

escape _V;
]],
    props = 'line 16 : not permitted inside an interface',
}
Test { [[
input void OS_START;
native _f, _V;
native do
    int V = 1;
end

class V with
do
    do finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V vv1;
    v = &&vv1;
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U&& u=null;
do
    var U uu;
    u = &&uu;
    await FOREVER;
end

do
    var Tx t;
end

escape _V;
]],
    run = 3,
}

Test { [[
input void OS_START;
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U u;
do
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

do
    var Tx t;
end

escape _V;
]],
    props = 'line 27 : not permitted inside an interface',
}
Test { [[
input void OS_START;
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
    if v? then end
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U&& u=null;
do
    var U uu;
    u = &&uu;
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

do
    var Tx t;
end

escape _V;
]],
    run = 3,
}

Test { [[
class V with
do
end
class U with
do
    var V&&? vv = spawn V;
end
var U u;
escape 2;
]],
    run = 2,
}

Test { [[
input void OS_START;

class V with
do
end

class U with
do
    var V&&? vv = spawn V;
end


var U t;
await OS_START;

native/nohold _tceu_trl, _tceu_trl_, _sizeof;
escape 2;
]],
    run = 2,
}

Test { [[
input void OS_START;
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
do        v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U u;
do
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

do
    var Tx t;
    await OS_START;
end

escape _V;
]],
    props = 'line 27 : not permitted inside an interface',
}
Test { [[
input void OS_START;
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
    if v? then end
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U&& u=null;
do
    var U uu;
    u = &&uu;
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

do
    var Tx t;
    await OS_START;
end

escape _V;
]],
    run = 3,
}

Test { [[
input void OS_START;
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& x=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U u;
do
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

var Tx t;
do
    await OS_START;
    var V&& v = t.u.x;
end

escape _V;
]],
    --fin = 'line 37 : pointer access across `await´',
    props = 'line 27 : not permitted inside an interface',
}
Test { [[
input void OS_START;
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
    if v? then end;
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U&& u=null;
do
    var U uu;
    u = &&uu;
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

var Tx t;
do
    await OS_START;
    var V&& v = t.u:v;
    if v==null then end;
end

escape _V;
]],
    --fin = 'line 39 : pointer access across `await´',
    run = 1,
}

Test { [[
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U u;
do
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

input void OS_START;

var Tx t;
do
    var U u;
    u.v = t.u.v;
    await OS_START;
end

escape _V;
]],
    --fin = 'line 38 : pointer access across `await´',
    props = 'line 26 : not permitted inside an interface',
    --fin = 'line 38 : organism pointer attribution only inside constructors',
}
Test { [[
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U u;
do
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

input void OS_START;

var Tx t;
do
    var U u;
    u.v = t.u.v;
    await OS_START;
end

escape _V;
]],
    --fin = 'line 38 : pointer access across `await´',
    --fin = 'line 38 : attribution to pointer with greater scope',
    props = 'line 26 : not permitted inside an interface',
    --fin = 'line 38 : organism pointer attribution only inside constructors',
}
Test { [[
native _f, _V;
native do
    int V = 1;
    int* f (){ escape NULL; }
end

class V with
do
    var& int? v;
    if v? then end;
        do v = &_f();
    finalize with
        _V = _V+1;
    end
    await FOREVER;
end

class U with
    var V&& v=null;
do
    var V&&? vv = spawn V;
    await FOREVER;
end

class Tx with
    var U&& u=null;
do
    var U uu;
    u = &&uu;
    //u.v = spawn V;
    var V&&? v = spawn V;
    await FOREVER;
end

input void OS_START;

var Tx t;
do
    var U u;
    u.v = t.u:v;
    await OS_START;
end

escape _V;
]],
    --fin = 'line 40 : pointer access across `await´',
    --fin = 'line 40 : organism pointer attribution only inside constructors',
    run = 2,
}

Test { [[
class V with
do
end

class Tx with
    var V&&? v;
do
    await 1s;
    v = spawn V;
end

var Tx t;
await 1s;
escape 1;

]],
    --fin = 'line 9 : invalid block for awoken pointer "v"',
    --fin = 'line 9 : pointer access across `await´',
    run = { ['~>1s']=1, }
}

Test { [[
class V with
do
end

input void OS_START;
class U with
    var V&&? v;
do
end

class Tx with
    var U&& u=null;
do
    await OS_START;
    u:v = spawn V;
end

do
    var U u;
    var Tx t;
        t.u = &&u;
    await OS_START;
end

escape 10;
]],
    --run = 10,
    env = 'line 15 : invalid attribution (no scope)',
}
Test { [[
class V with
do
end

input void A, OS_START;
class U with
    var V&&? v;
do
    await A;
end

class Tx with
    var U&& u=null;
do
    await OS_START;
    u:v = spawn V;
end

do
    var U u;
    var Tx t;
        t.u = &&u;
    await OS_START;
end

escape 10;
]],
    --run = { ['~>A']=10 },
    env = 'line 16 : invalid attribution',
}

Test { [[
native _V, _assert;
native do
    int V = 1;
end

class V with
do
    _V = 20;
    _V = 10;
end

class U with
    var V&&? v;
do
end

class Tx with
    var U&& u=null;
do
    await 1s;
    u:v = spawn V;
end

do
    var U u;
    do              // 26
        var Tx t;
        t.u = &&u;
        await 2s;
    end
    _assert(_V == 10);
end
_assert(_V == 10);
escape _V;
]],
    --run = { ['~>2s']=10, }       -- TODO: stack change
    env = 'line 21 : invalid attribution',
}

Test { [[
native do ##include <assert.h> end
native _assert;
native _V;
native do
    int V = 10;
end
class Tx with
do
    do finalize with
        _V = _V - 1;
    end
    await 500ms;
    _V = _V - 1;
end

do
    var Tx&&? a;
    a = spawn Tx;
    //free a;
    _assert(_V == 10);
    await 1s;
    _assert(_V == 8);
end

escape _V;
]],
    run = { ['~>1s']=8 },
}

Test { [[
native do ##include <assert.h> end
native _assert;
native _V;
native do
    int V = 10;
end
class Tx with
do
    do finalize with
        _V = _V - 1;
    end
    await 500ms;
    _V = _V - 1;
end

do
    pool[] Tx ts;
    var Tx&&? a;
    a = spawn Tx in ts;
    //free a;
    _assert(_V == 10);
end
_assert(_V == 9);

escape _V;
]],
    run = 9,
}

Test { [[
native do ##include <assert.h> end
native _assert;
native _X, _Y;
native do
    int X = 0;
    int Y = 0;
end

class Tx with
do
    do finalize with
        _Y = _Y + 1;
    end
    _X = _X + 1;
    await FOREVER;
end

do
    var Tx&&? ptr;
    loop i in [0 |> 100[ do
        if ptr? then
            //free ptr;
        end
        ptr = spawn Tx;
    end
    _assert(_X == 100 and _Y == 0);
end

_assert(_X == 100 and _Y == 0);
escape 10;
]],
    --loop = true,
    --fin = 'line 24 : invalid block for awoken pointer "ptr"',
    run = 10,
}

Test { [[
native do ##include <assert.h> end
native _assert;
native _X, _Y;
native do
    int X = 0;
    int Y = 0;
end

class Tx with
do
    do finalize with
        _Y = _Y + 1;
    end
    _X = _X + 1;
    await FOREVER;
end

do
    pool[] Tx ts;
    var Tx&&? ptr;
    loop i in [0 |> 100[ do
        if ptr? then
            //free ptr;
        end
        ptr = spawn Tx in ts;
    end
    _assert(_X == 100 and _Y == 0);
end

_assert(_X == 100 and _Y == 100);
escape 10;
]],
    --loop = true,
    --fin = 'line 24 : invalid block for awoken pointer "ptr"',
    run = 10,
}

Test { [[
native do ##include <assert.h> end
native _assert;
native _X, _Y;
native do
    int X = 0;
    int Y = 0;
end

class Tx with
do
    do finalize with
        _Y = _Y + 1;
    end
    _X = _X + 1;
    await FOREVER;
end

do
    var Tx&&? ptr;
    loop i in [0 |> 100[ do
        if ptr? then
            //free ptr;
        end
        ptr = spawn Tx;
    end
    _assert(_X == 100 and _Y == 99);
end

_assert(_X == 100 and _Y == 100);
escape 10;
]],
    todo = 'free',
    --loop = true,
    run = 10,
}

Test { [[
class U with do end;
class Tx with
    var U&& u;
do
end

do
    var U u;
    spawn Tx with
        this.u = &&u;
    end;
end
escape 1;
]],
    --fin = 'line 10 : attribution requires `finalize´',
    --fin = 'line 10 : attribution to pointer with greater scope',
    run = 1,
}

Test { [[
class U with do end;
class Tx with
    var& U u;
do
end

do
    var U u;
    spawn Tx with
        this.u = &u;
    end;
end
escape 1;
]],
    --fin = 'line 10 : attribution requires `finalize´',
    --run = 1,
    --ref = 'line 10 : attribution to reference with greater scope',
    ref = 'line 10 : invalid attribution : variable "u" has narrower scope than its destination',
}

Test { [[
class U with do end;
class Tx with
    var U&& u;
do
end

    var U u;
    spawn Tx with
        this.u = &&u;
    end;
escape 1;
]],
    run = 1,
}

Test { [[
class U with do end;
class Tx with
    var& U u;
do
end

    var U u;
    spawn Tx with
        this.u = &u;
    end;
escape 1;
]],
    run = 1,
}

Test { [[
class U with do end;
class Tx with
    var U&& u;
do
    var U&& u1 = u;
    if u1==null then end;
    await 1s;
end

do
    var U u;
    spawn Tx with
        this.u = &&u;
    end;
end
escape 1;
]],
    --fin = 'line 12 : attribution requires `finalize´',
    --fin = 'line 12 : attribution to pointer with greater scope',
    run = 1,
}

Test { [[
class U with do end;
class Tx with
    var U&& u;
do
    var U&& u1 = u;
    if u1==null then end;
    await 1s;
end

    var U u;
    spawn Tx with
        this.u = &&u;
    end;
escape 1;
]],
    run = 1,
}

Test { [[
class U with do end;
class Tx with
    var U&& u;
do
    var U&& u1 = u;
    await 1s;
    var U&& u2 = u;
end

do
    var U u;
    spawn Tx with
        this.u = &&u;
    end;
end
escape 1;
]],
    fin = 'line 7 : unsafe access to pointer "u" across `await´',
}

Test { [[
class Rect with
do
    await FOREVER;
end

var int n = 0;

par/or do
    await 1s;
with
    pool[1000] Rect rs;
    every 40ms do
        loop i in [0 |> 40[ do
            n = n + 1;
            spawn Rect in rs;
        end
    end
end
escape n;
]],
    run = { ['~>1s']=960 },
}

-- TODO: mem out e mem ever
--[=[
Test { [[
var void&& ptr;
class Tx with
do
end
loop i in [0 |> 100000[ do
    ptr = spawn Tx;
end
escape 10;
]],
    --loop = true,
    run = 10;
}
]=]

Test { [[
native do ##include <assert.h> end
native _V, _assert;
native do
    int V = 0;
end
class Tx with
    var int v=0;
do
    do finalize with
        do
            loop i in [0 |> 1[ do
                do break; end
            end
            _V = _V + this.v;
        end
    end
    await FOREVER;
end
do
    pool[] Tx ts;
    var Tx&&? p;
    p = spawn Tx in ts;
    p!:v = 1;
    p = spawn Tx in ts;
    p!:v = 2;
    p = spawn Tx in ts;
    p!:v = 3;
    input void OS_START;
    await OS_START;
end
_assert(_V == 6);
escape _V;
]],
    wrn = true,
    run = 6,
}

Test { [[
class Tx with
do
end
var Tx&&? t;
t = spawn Tx;
escape t!.v;
]],
    env = 'line 6 : not a struct',
}

Test { [[
native/plain _void;
class Tx with
    var int v=0;
do
    await FOREVER;
end

var _void&&[10] ts = [];
var Tx&&? t;
t = spawn Tx;
t!:v = 10;
ts[0] = (t!) as void&&;
escape t!:v + (ts[0] as Tx&&):v;
]],
    fin = 'line 12 : unsafe access to pointer "ts" across `spawn´ (tests.lua : 10)',
}

Test { [[
pre native do
    typedef void* void_;
end
native/plain _void_;
class Tx with
    var int v=0;
do
    await FOREVER;
end

var _void_[10] ts = [];
var Tx&&? t;
t = spawn Tx;
t!:v = 10;
ts[0] = (t!) as void&&;
escape t!:v + (ts[0] as Tx&&):v;
]],
    run = 20,
}

Test { [[
native _V;
native do
    int V = 0;
end

class Tx with
do
    _V = _V + 1;
    par/and do
        await 10ms;
    with
        loop i in [0 |> 5[ do
            if i==2 then
                break;
            end
            await 10ms;
        end
    end
    _V = _V + 1;
end

do
    loop i in [0 |> 10[ do
        await 1s;
        spawn Tx;
    end
    await 5s;
end

escape _V;
]],
    run = { ['~>1min']=20 },
}

Test { [[
class Tx with
    var int&& ptr = null;
do
end
do
    var int&& p = null;
    var Tx&&? ui = spawn Tx with
        this.ptr = p;   // ptr > p
    end;
end
escape 10;
]],
    --fin = 'line 8 : attribution requires `finalize´',
    --fin = 'line 8 : attribution to pointer with greater scope',
    run = 10,
}

Test { [[
class Tx with
    var void&& ptr = null;
do
end
do
    pool[] Tx ts;
    var void&& p = null;
    var Tx&&? ui;
    ui = spawn Tx in ts with
        this.ptr = p;
    end;
end
escape 10;
]],
    --fin = 'line 10 : attribution to pointer with greater scope',
    run = 10,
}

Test { [[
native _s;
pre native do
    typedef int s;
end

class Tx with
    var _s&& ptr = null;
do
end

do
    var _s&& p = null;
    var Tx&&? ui = spawn Tx with
        this.ptr = p;
    end;
end

escape 10;
]],
    run = 10,
    --fin = 'line 14 : attribution to pointer with greater scope',
    --fin = 'line 14 : attribution requires `finalize´',
}

Test { [[
native _s;
pre native do
    typedef int s;
end

class Tx with
    var _s&& ptr = null;
do
end

var Tx&&? ui;
do
    var _s&& p = null;
    do
        ui = spawn Tx with
            this.ptr = p;
        end;
    end
end

escape 10;
]],
    run = 10,
    --fin = 'line 16 : attribution to pointer with greater scope',
    --fin = 'line 16 : attribution requires `finalize´',
}

Test { [[
native _s;
pre native do
    typedef int s;
end

class Tx with
    var _s&& ptr = null;
do
end

do
    loop i in [0 |> 10[ do
        var _s&& p = null;
        spawn Tx with
            this.ptr = p;
        end;
        await 1s;
    end
end

escape 1;
]],
    run = { ['~>1min']=1 },
    --fin = 'line 15 : attribution to pointer with greater scope',
    --fin = 'line 15 : attribution requires `finalize´',
}
-- TODO: STACK
Test { [[
native do
    int V = 0;
end

class Tx with
    event void a;
do
    par/or do
        await a;
        _V = _V + 2;
    with
        emit a;
        _V = _V + 20;
    end
    await a;
    _V = _V + 10;
end

var Tx t;
_V = _V * 2;
emit t.a;
escape _V;
]],
    _ana = { acc=1 },
    --run = 14,
    run = 40,
}

Test { [[
native _s;
pre native do
    typedef int s;
end

class Tx with
    var _s&& ptr = null;
do
    _V = _V + 1;
end

native do ##include <assert.h> end
native _V, _assert;
native do
    int V=0;
end

do
    loop i in [0 |> 10[ do
        var _s&& p = null;
        spawn Tx with
            this.ptr = p;
        end;
        await 1s;
    end
    _assert(_V == 10);
end

escape _V;
]],
    run = { ['~>1min']=10 },
    --fin = 'line 22 : attribution to pointer with greater scope',
    --props = 'line 23 : not permitted inside a constructor',
}

Test { [[
class Tx with
    var void&& ptr;
do
end

var Tx t with
        do this.ptr = _malloc(10);
    finalize with
        _free();
    end
end;
]],
    --locs = 'line 22 : internal identifier "_" is not declared',
    fin = 'line 7 : constructor cannot contain `finalize´',
    --props = 'line 23 : not permitted inside a constructor',
}

Test { [[
class Tx with
    var void&& ptr;
do
end

spawn Tx with
        do this.ptr = _malloc(10);
    finalize with
        _free();
    end
end;
]],
    --locs = 'line 22 : internal identifier "_" is not declared',
    fin = 'line 7 : constructor cannot contain `finalize´',
    --props = 'line 23 : not permitted inside a constructor',
}

Test { [[
native _s;
pre native do
    typedef int s;
end

class Tx with
    var _s&& ptr = null;
do
end

native do ##include <assert.h> end
native _V, _assert;
native do
    int V=0;
end

do
    loop i in [0 |> 10[ do
        var _s&& p = null;
        spawn Tx with
                do this.ptr = p;
            finalize with
                _V = _V + 1;
            end
        end;
        await 1s;
    end
    _assert(_V == 10);
end

escape _V;
]],
    --locs = 'line 22 : internal identifier "_" is not declared',
    fin = 'constructor cannot contain `finalize´',
    --props = 'line 23 : not permitted inside a constructor',
}

Test { [[
native _s, _V;
    var _s&& p = null;
    loop i in [0 |> 10[ do
        var _s&& p1 = p;
        await 1s;
    end

escape _V;
]],
    run = { ['~>1min']=10 },
    fin = 'line 3 : unsafe access to pointer "p" across `loop´ (tests.lua : 2)',
}

Test { [[
native _s, _V;
pre native do
    typedef int s;
end

class Tx with
    var _s&& ptr = null;
do
    _V = _V + 1;
end

native do ##include <assert.h> end
native _V, _assert;
native do
    int V=0;
end

var Tx&&? ui;
do
    loop i in [0 |> 10[ do
        var _s&& p = null;
        ui = spawn Tx with
            this.ptr = p;
        end;
        await 1s;
    end
    _assert(_V == 10);
end

escape _V;
]],
    run = { ['~>1min']=10 },
    --fin = 'line 23 : attribution to pointer with greater scope',
}

Test { [[
native _s;
pre native do
    typedef int s;
end

class Tx with
    var _s&& ptr = null;
do
end

native do ##include <assert.h> end
native _V, _assert;
native do
    int V=0;
end

var Tx&&? ui;
do
    var _s&& p = null;
    loop i in [0 |> 10[ do
        ui = spawn Tx with
                do this.ptr = p;
            finalize with
                _V = _V + 1;
            end
        end;
        await 1s;
    end
    _assert(_V == 0);
end
//_assert(_V == 10);

escape _V;
]],
    --locs = 'line 23 : internal identifier "_" is not declared',
    fin = 'constructor cannot contain `finalize´',
    --props = 'line 24 : not permitted inside a constructor',
}

Test { [[
native _s;
pre native do
    typedef int s;
end

class Tx with
    var _s&& ptr = null;
do
end

native do ##include <assert.h> end
native _V, _assert;
native do
    int V=0;
end

do
    loop i in [0 |> 10[ do
        var _s&& p = null;
        var Tx&&? ui = spawn Tx with
                do this.ptr = p;   // p == ptr
            finalize with
                _V = _V + 1;
            end
        end;
        await 1s;
    end
    _assert(_V == 10);
end

escape _V;
]],
    --locs = 'line 22 : internal identifier "_" is not declared',
    fin = 'constructor cannot contain `finalize´',
    --fin = 'line 21 : invalid `finalize´',
}

Test { [[
class Game with
    event (int,int) go;
do
end

var Game game;
par/or do
    var int a,b;
    (a, b) = await game.go;
    if a and b then end;
with
    nothing;
end
escape 1;
]],
    run = 1;
}

Test { [[
class Game with
    event (int,int,int&&) go;
do
end

var Game game;
emit game.go => (1, 1, null);
escape 1;
]],
    env = 'line 2 : invalid event type',
    --env = 'line 7 : wrong argument #3 : cannot pass pointers',
    --run = 1;
}

Test { [[
class Unit with
    event int move;
do
end
var Unit&& u=null;
do
    var Unit unit;
    u = &&unit;
    await 1min;
end
emit u:move => 0;
escape 2;
]],
    --fin = 'line 8 : attribution requires `finalize´',
    fin = 'line 8 : attribution to pointer with greater scope',
    --fin = 'line 11 : pointer access across `await´',
}

Test { [[
class Unit with
    event int move;
do
end
var Unit&&? u;
do
    pool[] Unit units;
    u = spawn Unit in units;  // deveria falhar aqui!
    await 1min;
end
emit u!:move => 0;
escape 2;
]],
    run = {['~>1min']='12] runtime error: invalid tag'},
    --fin = 'line 11 : unsafe access to pointer "u" across `await´',
}

Test { [[
class Tx with do end;
pool[] Tx ts;
loop t in ts do
end
escape 1;
]],
    run = 1,
}

-- PAUSE/IF w/ ORGS

Test { [[
input void OS_START;
input int A,B;

class Tx with
    event int e;
do
    var int v = await A;
    emit e => v;
end

event int a;

var int ret=0;
par/or do
    pause/if a do
        var Tx t;
        ret = await t.e;
    end
with
    await OS_START;
    emit a => 1;
    await B;
    emit a => 0;
    await FOREVER;
end
escape ret;
]],
    run = { ['10~>A; 1~>B; 5~>A'] = 5 },
}

Test { [[
input void A,X, OS_START;
event int a;//=0;
var int ret = 0;

class Tx with
    event void v, ok, go;
do
    await A;
    emit v;
    emit ok;
end

par/or do
    pause/if a do
        vector[2] Tx ts;
        par/or do
            par/and do
                await ts[0].ok;
            with
                await ts[1].ok;
            end
        with
            par do
                await ts[0].v;
                ret = ret + 1;
            with
                await ts[1].v;
                ret = ret + 1;
            end
        end
    end
with
    await OS_START;
    emit a => 1;
    await X;
    emit a => 0;
    ret = 10;
    await FOREVER;
end
escape ret;
]],
    _ana = {
        reachs = 1,
        --acc = 3,  -- TODO
    },
    run = { ['~>A; ~>X; ~>A']=12 }
}

Test { [[
native _V;
native do
    int V = 0;
end

class Tx with
    var int c;
do
    do finalize with
        _V = _V + c;
    end
    await FOREVER;
end

par/or do
    do
        pool[] Tx ts;
        loop i do
            spawn Tx in ts with
                this.c = i;
            end;
            await 1s;
        end
    end
with
    await 5s;
end

escape _V;
]],
    run = { ['~>5s']=15 },
}

Test { [[
native _V;
native do
    int V = 0;
end

class Tx with
    var int c;
do
    do finalize with
        _V = _V + c;
    end
    await FOREVER;
end

input int P;
event int pse;

par/or do
    pause/if pse do
        do
            pool[] Tx ts;
            loop i do
                spawn Tx in ts with
                    this.c = i;
                end;
                await 1s;
            end
        end
    end
with
    loop do
        var int v = await P;
        emit pse  =>  v;
    end
with
    await 5s;
end

escape _V;
]],
    wrn = true,
    run = { ['~>2s;1~>P;~>2s;0~>P;~>1s']=6 },
}

Test { [[
native _V;
native do
    int V = 0;
end

class Tx with
    var int c;
do
    do finalize with
        _V = _V + c;
    end
    await FOREVER;
end

input int P;
event int pse;

par/or do
    do
        pool[] Tx ts;
        loop i do
            pause/if pse do
                spawn Tx in ts with
                    this.c = i;
                end;
                await 1s;
            end
        end
    end
with
    loop do
        var int v = await P;
        emit pse  =>  v;
    end
with
    await 5s;
end

escape _V;
]],
    run = { ['~>2s;1~>P;~>2s;0~>P;~>1s']=6 },
}

Test { [[
native _V;
native do
    int V = 0;
end

class Tx with
    var int c;
do
    do finalize with
        _V = _V + c;
    end
    await 5s;
    _V = _V + 10;
end

input int P;
event int pse;

par/or do
    do
        pool[] Tx ts;
        loop i do
            pause/if pse do
                spawn Tx in ts with
                    this.c = i;
                end;
                await 1s;
            end
        end
    end
with
    loop do
        var int v = await P;
        emit pse  =>  v;
    end
with
    await 5s;   // terminates before first spawn
with
    await 5s;   // terminates before first spawn
end

escape _V;
]],
    run = { ['~>2s;1~>P;~>2s;0~>P;~>2s']=16 },
}
Test { [[
native _V;
native do
    int V = 0;
end

class Tx with
    var int c;
do
    do finalize with
        _V = _V + c;
    end
    await 5s;
    _V = _V + 10;
end

input int P;
event int pse;

par/or do
    pause/if pse do
        do
            pool[] Tx ts;
            loop i do
                spawn Tx in ts with
                    this.c = i;
                end;
                await 1s;
            end
        end
    end
with
    loop do
        var int v = await P;
        emit pse  =>  v;
    end
with
    await 5s;   // terminates before first spawn
end

escape _V;
]],
    wrn = true,
    run = { ['~>2s;1~>P;~>2s;0~>P;~>1s']=6 },
}

Test { [[
native _V;
native do
    int V = 0;
end

class Tx with
    var int c;
do
    do finalize with
        _V = _V + c;
    end
    await 5s;
    _V = _V + 10;
end

input int P;
event int pse;

par/or do
    do
        pool[] Tx ts;
        loop i do
            pause/if pse do
                spawn Tx in ts with
                    this.c = i;
                end;
                await 1s;
            end
        end
    end
with
    loop do
        var int v = await P;
        emit pse  =>  v;
    end
with
    await 6s;
end

escape _V;
]],
    run = { ['~>2s;1~>P;~>2s;0~>P;~>2s']=30 },
}

Test { [[
native _V;
native do
    int V = 0;
end

class Tx with
    var int c;
do
    do finalize with
        _V = _V + c;
    end
    await 5s;
    _V = _V + 10;
end

input int P;
event int pse;

par/or do
    pause/if pse do
        do
            pool[] Tx ts;
            loop i do
                spawn Tx in ts with
                    this.c = i;
                end;
                await 1s;
            end
        end
    end
with
    loop do
        var int v = await P;
        emit pse  =>  v;
    end
with
    await 5s;
end

escape _V;
]],
    wrn = true,
    run = { ['~>2s;1~>P;~>2s;0~>P;~>1s']=6 },
}

Test { [[
event int a;
input void A;
var int v = 0;

class Tx with
    event int a;
do
    pause/if a do
        await FOREVER;
    end
end

par/or do
    pause/if a do
        await FOREVER;
    end
with
    loop do
        await 1s;
        v = v + 1;
    end
with
    var Tx t;
    var int pse_ = 0;
    loop do
        await 1s;
        pse_ = not pse_;
        emit a => pse_;
        emit t.a => pse_;
    end
with
    await A;
end
escape v;
]],
    _ana = { acc=0 },
    run = { ['~>10s;~>A']=10 }
}

Test { [[
class Tx with
    var int v;
do
    await FOREVER;
end
var Tx&&? t = spawn Tx with
             this.v = 10;
           end;
//free(t);
escape t!:v;
]],
    run = 10,
}

-- kill org inside iterator
Test { [[
class Tx with
    event void e;
do
    await e;
end

pool[] Tx ts;

var int ret = 1;

spawn Tx in ts;
async do end;

loop t in ts do
    watching *t do
        ret = ret + 1;
        emit t:e;
        ret = ret + 1;
    end
end

escape ret;
]],
    props = 'line 14 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 2,
}

Test { [[
class Tx with
    var Tx&& t;
    event void e;
do
    watching *t do
        await e;
    end
end

pool[] Tx ts;

var int ret = 1;

var Tx&&? t1 = spawn Tx in ts with
                this.t = &&this;
            end;
var Tx&&? t2 = spawn Tx in ts with
                this.t = t1!;
            end;

async do end;

loop t in ts do
    watching *t do
        ret = ret + 1;
        emit t:e;
        ret = ret + 1;
    end
end

escape ret;
]],
    props = 'line 23 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 2,
}

Test { [[
interface I with
    var int v;
    event void e;
end
class Tx with
    interface I;
do
    await e;
end
pool[] Tx ts;
var int ret = 0;
do
    spawn Tx in ts with
        this.v = 10;
    end;
    async do end;
    loop t in ts do
        watching *t do
            ret = ret + t:v;
            emit t:e;
            ret = ret + t:v;
        end
    end
end
escape ret;
]],
    props = 'line 17 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 10,
}

Test { [[
class Tx with
    event void e;
do
    await e;
end

pool[] Tx ts;

var int ret = 1;

spawn Tx in ts;
spawn Tx in ts;
async do end;

loop t1 in ts do
    loop t2 in ts do
        ret = ret + 1;
    end
end

escape ret;
]],
    run = 5,
}
Test { [[
class Tx with
    event void e;
do
    await e;
end

pool[] Tx ts;

var int ret = 1;

spawn Tx in ts;
spawn Tx in ts;
async do end;

loop t1 in ts do
    loop t2 in ts do
        ret = ret + 1;
        kill *t2;
    end
end

escape ret;
]],
    props = 'line 15 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 3,
}
Test { [[
class Tx with
    event void e;
do
    await e;
end

pool[] Tx ts;

var int ret = 1;

spawn Tx in ts;
spawn Tx in ts;
async do end;

loop t1 in ts do
    watching *t1 do
        loop t2 in ts do
            watching *t2 do
                ret = ret + 1;
                kill *t1;
            end
        end
    end
end

escape ret;
]],
    props = 'line 15 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 3,
}

Test { [[
class Tx with
    event void e;
do
    await e;
end

pool[] Tx ts;

var int ret = 1;

spawn Tx in ts;
spawn Tx in ts;
async do end;

loop t1 in ts do
    watching *t1 do
        loop t2 in ts do
            ret = ret + 1;
            emit t1:e;
            ret = ret + 1;
        end
    end
end

escape ret;
]],
    props = 'line 15 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 3,
}

-- TODO pause hierarquico dentro de um org
-- SDL/samples/sdl4.ceu

-- INTERFACES / IFACES / IFCES

if COMPLETE then
    for i=120, 150 do
        local str = {}
        for j=1, i do
            str[#str+1] = [[
class Class]]..j..[[ with
    interface I;
do
    x = 10;
end
    ]]
        end
        str = table.concat(str)

        Test { [[
interface I with
    var int x;
end
]]..str..[[

var Class]]..i..[[ instance;
var I&& target = &&instance;
escape target:x;
]],
            run = 10,
        }
    end
end

Test { [[
interface A with
    var int a1,a2;
end
interface B with
    var int b1,b2;
end
interface C with
    var int c1,c2;
end
interface I with
    interface A;
    interface B,C;
end
class Tx with
    interface I;
do
    await FOREVER;
end
var Tx t with
    this.a1 = 1;
    this.a2 = 2;
    this.b1 = 3;
    this.b2 = 4;
    this.c1 = 5;
    this.c2 = 6;
end;
var I&&? i = &&t;
escape i!:a1+i!:a2+i!:b1+i!:b2+i!:c1+i!:c2;
]],
    run = 21,
}
Test { [[
interface A with
    var int a1,a2;
end
interface B with
    var int b1,b2;
end
interface C with
    var int c1,c2;
end
interface I with
    interface A;
    interface B,C;
end
class Tx with
    interface I;
do
end
var Tx t with
    this.a1 = 1;
    this.a2 = 2;
    this.b1 = 3;
    this.b2 = 4;
    this.c1 = 5;
    this.c2 = 6;
end;
var I&& i = &&t;
escape i:a1+i:a2+i:b1+i:b2+i:c1+i:c2;
]],
    run = 21,
}
Test { [[
interface I with
    var int a;
end
class Tx with
do end
do
    pool[] Tx ts;
    loop i in ts do
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
native _ptr;
native do
    void* ptr;
end
interface I with
    event void e;
end
var J&& i = _ptr;
escape 10;
]],
    env = 'line 8 : undeclared type `J´',
}

Test { [[
native _ptr;
native do
    void* ptr;
end
interface I with
    event void e;
end
var I&& i = _ptr;
escape 10;
]],
    --env = 'line 8 : invalid attribution',
    todo = 'i=ptr',
    run = 10,
}

Test { [[
native _ptr;
native do
    void* ptr;
end
interface I with
    event int e;
end
var I&& i = _ptr as I&&;
escape 10;
]],
    run = 10;
}

-- CAST

Test { [[
native do ##include <assert.h> end
native _assert;

interface Tx with
end

class T1 with
do
end

class T2 with
do
end

var T1 t1;
var T2 t2;
var Tx&& t;

t = &&t1;
var T1&& x1 = ( t as T1&&);
_assert(x1 != null);

t = &&t1;
var T2&& x2 = ( t as T2&&);
_assert(x2 == null);

escape 10;
]],
    run = 10;
}

Test { [[
interface I with
end
class Tx with
    var I&& parent;
do
end
class U with
do
    var Tx move with
        this.parent = &&outer;
    end;
end
escape 1;
]],
    run = 1,
}

Test { [[
interface I with
    event int a;
end
escape 10;
]],
    run = 10;
}

Test { [[
interface I with
end
vector[10] I a;
]],
    env = 'line 3 : cannot instantiate an interface',
}

Test { [[
interface I with
    var int i;
end

interface J with
    interface I;
end

var I&& i = null;
var J&& j = i;

escape 1;
]],
    run = 1,
}


-- GLOBAL

Test { [[
input void OS_START;
interface Global with
    event int a_;
end
event int a_;
class U with
    event int a_;
do
end
class Tx with
    event int a_;
    var Global&& g;
do
    await OS_START;
    emit g:a_  =>  10;
end
var U u;
var Global&& g = &&u;
var Tx t;
t.g = &&u;
var int v = await g:a_;
escape v;
]],
    todo = 'watching',
    run = 10,
}

Test { [[
interface Global with
    var int&& a;
end
var int&& a=null;
var int&& b=null;
b = global:a;
do
    var int&& c;
    c = global:a;
    if c == null then end;
end
escape 1;
]],
    run = 1,
}
Test { [[
interface Global with
    var int&& a;
end
var int&& a=null;
var int&& b=null;
global:a = b;       // don't use global
do
    var int&& c=null;
    global:a = c;
end
escape 1;
]],
    run = 1,
    fin = 'line 6 : attribution to pointer with greater scope',
    --fin = 'line 6 : organism pointer attribution only inside constructors',
}
Test { [[
interface Global with
    var int&& a;
end
var int&& a=null;
var int&& b=null;
global:a = b;       // don't use global
do
    var int&& c=null;
    await 1s;
    global:a = c;
end
escape 1;
]],
    fin = 'line 6 : attribution to pointer with greater scope',
    --fin = 'line 6 : organism pointer attribution only inside constructors',
}
Test { [[
interface Global with
    var int&& a;
end
var int&& a = null;
class Tx with
do
    var int&& b;
    b = global:a;
end
escape 1;
]],
    fin = 'line 8 : unsafe access to pointer "a" across `class´ (tests.lua : 5)',
    --run = 1,
}
Test { [[
interface Global with
    var int&& a;
end
var int&& a = null;
class Tx with
do
    var int&& b=null;
    global:a = b;
end
escape 1;
]],
    fin = 'line 8 : attribution to pointer with greater scope',
    --fin = 'line 7 : unsafe access to pointer "a" across `class´ (tests.lua : 4)',
    --fin = 'line 7 : organism pointer attribution only inside constructors',
}

Test { [[
input void OS_START;
interface Global with
    event int a;
end
event int a;
class Tx with
    event int a;
do
    await OS_START;
    emit global:a  =>  10;
end
var Tx t;
var int v = await a;
escape v;
]],
    run = 10,
}
Test { [[
input void OS_START;
interface Global with
    event int a;
    var int aa;
end
event int a;
var int aa = 0;
class Tx with
    event int a;
    var int aa=0;
do
    aa = await global:a;
end
var Tx t;
await OS_START;
emit a  =>  10;
escape t.aa;
]],
    run = 10,
}

Test { [[
interface Global with
    var int v;
end
var int v;

class Tx with
    var int v = 1;
do
    this.v = global:v;
end
var Tx t;

escape t.v;
]],
    ref = 'line 4 : uninitialized variable "v" crossing compound statement (tests.lua:6)',
    --ref = 'line 9 : invalid access to uninitialized variable "v" (declared at tests.lua:2)',
}

Test { [[
interface Global with
    var int v;
end
var int v;

class Tx with
    var int v = 1;
do
    //this.v = global:v;
end
var Tx t;

escape v;
]],
    ref = 'line 4 : uninitialized variable "v" crossing compound statement (tests.lua:6)',
    --ref = 'line 13 : invalid access to uninitialized variable "v" (declared at tests.lua:4)',
}

Test { [[
interface Global with
    var int v;
end
var int v;

class Tx with
    var int v = 1;
do
    //this.v = global:v;
end
var Tx t;

escape t.v;
]],
    ref = 'line 4 : uninitialized variable "v" crossing compound statement (tests.lua:6)',
    --run = 1,
}

Test { [[
interface Global with
    var int v;
end
var int v=0;

class Tx with
    var int v = 1;
do
    //this.v = global:v;
end
var Tx t;

escape t.v;
]],
    run = 1,
}

Test { [[
interface Global with
    var int v;
end

class Tx with
    var int v = 1;
do
    //this.v = global:v;
end
var Tx t;

var int v=0;
escape t.v;
]],
    run = 1,
}

-- use of global before its initialization
Test { [[
interface Global with
    var& int vvv;
end

class Tx with
    var int v = 1;
do
    this.v = global:vvv;
end
var Tx t;

var int  um = 111;
var& int vvv = &um;
escape t.v;
]],
    ref = 'line 8 : invalid access to uninitialized variable "vvv" (declared at tests.lua:2)',
}

Test { [[
class Tx with
    var int a=0;
do
    a = global:a;
end
var int a = 10;
var Tx t;
input void OS_START;
await OS_START;
t.a = t.a + a;
escape t.a;
]],
    env = 'line 4 : interface "Global" is not defined',
}

Test { [[
interface Global with
    var int a;
end
class Tx with
    var int a=0;
do
    a = global:a;
end
do
    var int a = 10;
    var Tx t;
input void OS_START;
await OS_START;
    t.a = t.a + a;
    escape t.a;
end
]],
    env = 'line 1 : interface "Global" must be implemented by class "Main"',
}

Test { [[
interface Global with
    var int a;
end
var int a = 10;
class Tx with
    var int a=0;
do
    a = global:a;
end
do
    var Tx t;
input void OS_START;
await OS_START;
    t.a = t.a + a;
    escape t.a;
end
]],
    run = 20,
}

Test { [[
native/nohold _attr;
native do
    void attr (void* org) {
        IFC_Global_a() = CEU_T_a(org) + 1;
    }
end

interface Global with
    var int a;
end
class Tx with
    var int a=0;
do
    a = global:a;
    _attr(this);
    a = a + global:a + this.a;
end
var int a = 10;
do
    var Tx t;
input void OS_START;
await OS_START;
    t.a = t.a + a;
    escape t.a + global:a;
end
]],
    todo = 'IFC accs',
    run = 53,
}

Test { [[
native do
    int fff (CEU_T* t, int v) {
        escape CEU_T_fff(NULL, t, v);
    }
    int iii (CEU_III* i, int v) {
        escape CEU_III__fff(i)(NULL, i, v);
    }
    int vvv (CEU_III* i) {
        escape *CEU_III__vvv(i);
    }
end
native/pure _fff, _iii, _vvv;

interface III with
    var int vvv;
    code/instantaneous Fff (var int)=>int;
end

class Tx with
    var int vvv;
    code/instantaneous Fff (var int)=>int;
do
    code/instantaneous Fff (var int v)=>int do
        escape this.vvv + v;
    end
    await FOREVER;
end

var Tx t with
    this.vvv = 100;
end;

var III&& i = &&t;

escape t.fff(10) + _fff(&&t, 10) + _iii(i, 10) + _vvv(i);
]],
    run = 430,
}

Test { [[
native do
    int V = 10;
end

interface Global with
    event void e;
end
event void e;

class Tx with
do
    emit global:e;
    _V = 1;
end

par/or do
    event void a;
    par/or do
        await 1s;
        do
            var Tx t;
            emit a;
            _V = 1;
        end
    with
        await global:e;
    with
        await a;
    end
    await 1s;
with
    async do
        emit 1s;
    end
end
escape _V;
]],
    run = 10,
}
Test { [[
interface I with
    event int a;
end
var I t;
escape 10;
]],
    env = 'line 4 : cannot instantiate an interface',
}

Test { [[
interface Global with
    event void e;
end
event void e;

class Tx with
do
    emit global:e;
end

var int ret = 0;
par/or do
    await 1s;
    do
        var Tx t;
        await FOREVER;
    end
with
    await global:e;
    ret = 1;
with
    async do
        emit 1s;
    end
end
escape ret;
]],
    run = 1,
}
Test { [[
interface I with
    event int a;
end
vector[10] I t;
escape 10;
]],
    env = 'line 4 : cannot instantiate an interface',
}

Test { [[
interface I with
    event int a;
end
var I&&? t;
t = spawn I;
escape 10;
]],
    env = 'line 5 : cannot instantiate an interface',
}

Test { [[
class Tx with
do
end

interface I with
    event int a;
end

var I&& i;
var Tx t;
i = &&t;
escape 10;
]],
    env = 'line 11 : types mismatch',
}

Test { [[
class Tx with
    event void a;
do
end

interface I with
    event int a;
end

var I&& i;
var Tx t;
i = &&t;
escape 10;
]],
    env = 'line 12 : types mismatch',
}

Test { [[
class Tx with
    event int a;
do
end

interface I with
    event int a;
end

var I&& i;
var Tx t;
i = t;
escape 10;
]],
    env = 'line 12 : types mismatch',
}

Test { [[
class Tx with
    event int a;
do
end

interface I with
    event int a;
end

var I&& i;
var Tx t;
i = &&t;
escape 10;
]],
    run = 10,
    --ref = 'line 10 : uninitialized variable "i" crossing compound statement (tests.lua:11)',
}

Test { [[
class Tx with
    event int a;
do
end

interface I with
    event int a;
end

var Tx t;
var I&& i;
i = &&t;
escape 10;
]],
    run = 10;
}

Test { [[
class Tx with
    event int a;
do
end

interface I with
    event int a;
end
interface J with
    event int a;
end

var Tx t;
var I&& i;
i = &&t;
var J&& j = i;
escape 10;
]],
    run = 10;
}

Test { [[
class Tx with
    event int a;
do
end

interface I with
    event int a;
end
interface J with
    event u8 a;
end

var I&& i;
var Tx t;
i = &&t;
var J&& j = i;
escape 10;
]],
    env = 'line 16 : types mismatch',
}

Test { [[
class Tx with
    var int v;
    var int&& x;
    event int a;
do
    a = 10;
end

interface I with
    event int a;
end
interface J with
    event int a;
    var int v;
end

var I&& i;
var Tx t;
i = &&t;
var J&& j = i;
escape 0;
]],
    env = 'line 6 : types mismatch',
}

Test { [[
input void OS_START;
class Tx with
    event int a;
    var int aa=0;
do
    aa = 10;
end

interface I with
    event int a;
    var int aa;
end
interface J with
    event int a;
    var int aa;
end

var Tx t;
var I&& i;
i = &&t;
var J&& j = i;
escape i:aa + j:aa + t.aa;
]],
    run = 30,
}

Test { [[
input void OS_START;
class Tx with
    event int a;
    var int aa=0;
do
    aa = 10;
end

interface I with
    event int a;
    var int aa;
end
interface J with
    event int a;
    var int aa;
end

var Tx t;
var I&& i;
i = &&t;
var J&& j = i;
await OS_START;
escape i:aa + j:aa + t.aa;
]],
    fin = 'line 23 : unsafe access to pointer "i" across `await´',
}

Test { [[
input void OS_START;
class Tx with
    var int v=0;
    var int&& x=null;
    var int a=0;
do
    a = 10;
    v = 1;
end

interface I with
    var int a;
    var int v;
end
interface J with
    var int a;
end

var Tx t;
var I&& i;
i = &&t;
var J&& j = i;
escape i:a + j:a + t.a + i:v + t.v;
]],
    run = 32,
}

Test { [[
input void OS_START;
class Tx with
    var int v=0;
    var int&& x=null;
    var int a=0;
do
    a = 10;
    v = 1;
end

interface I with
    var int a;
    var int v;
end
interface J with
    var int a;
end

var Tx t;
var I&& i;
i = &&t;
var J&& j = i;
await OS_START;
escape i:a + j:a + t.a + i:v + t.v;
]],
    fin = 'line 24 : unsafe access to pointer "i" across `await´',
    --run = 32,
}

Test { [[
class Sm with
do
end
interface Media with
    var Sm sm;
end
escape 10;
]],
    props = 'line 5 : not permitted inside an interface',
}
Test { [[
class Sm with
do
end
interface Media with
    var Sm&& sm;
end
escape 10;
]],
    run = 10;
}

Test { [[
interface I with
    var int a;
end
class Tx with
    interface J;
do
    a = 10;
end
var Tx t;
escape t.a;
]],
    adj = 'line 5 : interface "J" is not declared',
}

Test { [[
interface MenuGamesListener with
    event int ok_rm;
    event int ok_go;
end
class MenuGames with
    interface MenuGamesListener;
do
    var MenuGamesListener&& lst = &&this;
    if lst==null then end;
end
escape 1;
]],
    run = 1,
}

Test { [[
interface I with
    var int a;
end
class Tx with
    interface I;
    var int a=0;
do
    a = 10;
end
var Tx t;
input void OS_START;
await OS_START;
escape t.a;
]],
    run = 10,
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var int v;
do
end

var Tx t;
    t.v = 10;
var I&& i = &&t;
escape t._ins();
]],
    --env = 'line 13 : native function "CEU_T__ins" is not declared',
    locs = 'line 13 : internal identifier "_ins" is not declared',
}
Test { [[
interface I with
    var int v;
end

class Tx with
    var int v;
do
end

var Tx t;
    t.v = 10;
var I&& i = &&t;
escape i:_ins();
]],
    --env = 'line 13 : native function "CEU_I__ins" is not declared',
    locs = 'line 13 : internal identifier "_ins" is not declared',
}
Test { [[
class Tx with do end
class U with
    interface Tx;
do
end
escape 0;
]],
    adj = 'line 3 : interface "Tx" is not declared',
}

Test { [[
interface Global with
    var Gx&& g;
end
var Gx&& g;
escape 1;
]],
    env = 'line 2 : undeclared type `Gx´',
}

Test { [[
interface Global with
    event (Gx&&,int) g;
end
event (Gx&&,int) g;
escape 1;
]],
    env = 'line 2 : invalid event type'
    --env = 'line 2 : undeclared type `Gx´',
    --run = 1,
    --gcc = '22:2: error: unknown type name ‘Gx’',
    --gcc = 'error: unknown type name',
}

Test { [[
interface I with
native _char;
    var _char c;
end
class Tx with
    interface I;
    var _char c=0;
do
    this.c = 1;
end
var Tx t;
var I&& i = &&t;
escape i:c == 1;
]],
    run = 1,
}

-- XXX: Tx-vs-Opt

Test { [[
input _vldoor_t&& T_VERTICAL_DOOR;
class T_VerticalDoor with
    var void&& v;
do
end

do
    every door in T_VERTICAL_DOOR do
        spawn T_VerticalDoor with
            this.v = door;
        end;
    end
end
]],
    --env = 'line 11 : invalid attribution (void&& vs _vldoor_t&&)',
    --fin = 'line 11 : attribution to pointer with greater scope',
    --fin = 'line 9 : invalid block for awoken pointer "door"',
    _ana = {
        isForever = true,
    },
}

Test { [[
input _vldoor_t&& T_VERTICAL_DOOR;
class T_VerticalDoor with
    var void&& v;
do
end

do
    every door in T_VERTICAL_DOOR do
        spawn T_VerticalDoor with
            this.v = door as void&&;
        end;
    end
end
]],
    --fin = 'line 11 : attribution to pointer with greater scope',
    --fin = 'line 9 : invalid block for awoken pointer "door"',
    _ana = {
        isForever = true,
    },
}

Test { [[
class Tx with
    var void&& v=null;
do
end

var Tx t;
t.v = null;
var void&& ptr = null;
t.v = ptr;
escape 1;
]],
    --fin = 'line 9 : organism pointer attribution only inside constructors',
    --fin = 'line 9 : attribution to pointer with greater scope',
    run = 1,
}
Test { [[
class Tx with
    var void&& v;
do
end

var Tx t with
    this.v = null;
end;
var void&& ptr = null;
t.v = ptr;
escape 1;
]],
    --fin = 'line 10 : organism pointer attribution only inside constructors',
    --fin = 'line 9 : attribution to pointer with greater scope',
    run = 1,
}

Test { [[
class Tx with
    var void&& v=null;
do
end

var Tx t, s;
t.v = null;
t.v = s.v;
escape 1;
]],
    --fin = 'line 8 : organism pointer attribution only inside constructors',
    run = 1,
}

Test { [[
interface I with
end

interface Global with
    var I&& t;
end

var I&& t = null;

class Tx with
do
    global:t = &&this;
end

escape 1;
]],
    --fin = 'line 10 : attribution requires `finalize´'
    fin = 'line 12 : attribution to pointer with greater scope',
    --fin = 'line 10 : organism pointer attribution only inside constructors',
}

Test { [[
native do
    void* v;
end
class Tx with
    var& _void ptr;
do
end
var Tx t with
    this.ptr = &_v;
end;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var byte && str;
do
    str = "oioi";
    this.str = "oioi";
end
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
end
var int&& p1=null;
var& int&&  v=&p1;
var Tx&& p=null;
var& Tx&&  t=&p;
escape 1;
]],
    run = 1;
}

Test { [[
class Tx with
do
end
var& int && v;
var& Tx && t;
escape 1;
]],
    env = 'line 4 : invalid type modifier : `&&&´',
}

Test { [[
class Tx with
    var byte&& str;
do
end

do
    spawn Tx with
        var byte&& s = "str";
        this.str = s;
    end;
end

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var int&& v;
do
    *v = 1;
    await 1s;
    *v = 2;
end
escape 1;
]],
    fin = 'line 6 : unsafe access to pointer "v" across `await´',
}

Test { [[
interface Global with
    var int&& a;
end
var int&& a = null;
class Tx with
    var int&& v;
do
end
var Tx t with
    this.v = global:a;
end;
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
interface Global with
    var int&& a;
end
var int&& a = null;
class Tx with
    var int&& v;
do
end
await OS_START;
var Tx t with
    this.v = global:a;
end;
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
interface Global with
    var int&& p;
end
var int i = 1;
var int&& p = null;
await OS_START;
p = p;
escape *p;
]],
    fin = 'line 8 : unsafe access to pointer "p" across `await´',
}

Test { [[
input void OS_START;
interface Global with
    var int&& p;
end
var int i = 1;
var int&& p = null;
await OS_START;
p = &&i;
escape *p;
]],
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
    var int&& p;
do
end
var int i = 1;
var Tx t with
    this.p = null;
end;
await OS_START;
t.p = &&i;
escape *t.p;
]],
    run = 1,
    --fin = 'line 11 : unsafe access to pointer "p" across `await´',
}

Test { [[
native do
    void* V;
end
class Tx with
    code/instantaneous Fx (var void&& v)=>void;
do
    code/instantaneous Fx (var void&& v)=>void do
        _V := v;
    end
end
escape 1;
]],
    --fin = 'line 8 : invalid attribution',
    run = 1,
}

Test { [[
native/plain _pkt_t;
class Forwarder with
    var _pkt_t out = _pkt_t();
do
end

native/nohold _memcpy;

input _pkt_t&& RECEIVE;

every inc in RECEIVE do
    spawn Forwarder with
        _memcpy(&&this.out, inc, inc:len);
    end;
end
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
class Unit with
    var _SDL_Texture&& tex;
do
end

interface Global with
    pool[] Unit all;
end

pool[] Unit all;

class Nest with
do
    spawn Unit in global:all with
        this.tex := _TEX_STORMTROOPER;
    end;
end
]],
    fin = 'line 15 : wrong operator',
}

-- TODO_TYPECAST

-- IFACES / IFCS / ITERATORS

Test { [[
interface I with
    var int v;
end
class Tx with
    var& I parent;
    pool[1] I iss;
do
    if &&parent==null then end;
    await 1s;
end
escape 1;
]],
    run = 1,
}

Test { [[
interface I with end
class Tx with do end
pool[] Tx ts;
do
    loop i in ts do
native _f;
        _f(i);
    end
end
]],
    fin = 'line 6 : call requires `finalize´',
}

Test { [[
interface I with end
class Tx with do end
pool[] Tx ts;
var I&& p=null;
do
    loop i in ts do
        p = i;
    end
end
escape 1;
]],
    --fin = 'line 7 : attribution requires `finalize´',
    run = 1,
}

Test { [[
interface Unit with end
class CUnit with do end
pool[] CUnit us;
loop u in us do
end
escape 1;
]],
    run = 1,
}

Test { [[
class Unit with do end
pool[] Unit us;
var int ret = 1;
do
    loop u in us do
        ret = ret + 1;
    end
end
escape ret;
]],
    run = 1,
}

Test { [[
class Unit with do end
pool[] Unit us;
var Unit&& p=null;
do
    loop i in us do
        p = i;
    end
end
escape 10;
]],
    run = 10;
}

Test { [[
class I with do end
pool[] I iss;
native/nohold _f;
native do
    void f (void* p) {
    }
end
do
    loop i in iss do
        _f(i);
    end
end
escape 10;
]],
    run = 10,
}

Test { [[
class I with do end
pool[] I iss;
native _f;
native do
    void f (void* p) {
    }
end
do
    loop i in iss do
        do _f(i); finalize with nothing; end;
    end
end
escape 10;
]],
    run = 10,
}

Test { [[
interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
    await FOREVER;
end

pool[] Tx ts;
var int ret = 0;
do
    spawn Tx in ts with
        this.v = 1;
    end;
    spawn Tx in ts with
        this.v = 2;
    end;
    spawn Tx in ts with
        this.v = 3;
    end;

    loop i in ts do
        ret = ret + i:v;
    end
end
escape ret;
]],
    run = 6,
}

Test { [[
interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
    var int v=0;
do
    await FOREVER;
end

class U with
    var int v=0;
do
    await FOREVER;
end

var Tx t;
var U u;

var I&& i1 = &&t;
var I&& i2 = (&&u as I&&);

native/pure _f;
native do
    void* f (void* org) {
        escape org;
    }
end

var I&& i3 = ( _f(&&t) as I&&);
var I&& i4 = ( _f(&&u) as I&&);

var Tx&& i5 = ( _f(&&t) as Tx&&);
var Tx&& i6 = ( _f(&&u) as Tx&&);

escape i1==&&t and i2==null and i3==&&t and i4==null and i5==&&t and i6==null;
]],
    run = 1,
}
Test { [[
interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
    await FOREVER;
end
pool[] Tx ts;

class U with
    var int v;
do
    await FOREVER;
end

var int ret = 0;
do
    spawn Tx with
        this.v = 1;
    end;
    spawn U with
        this.v = 2;
    end;
    spawn Tx in ts with
        this.v = 3;
    end;

    loop i in ts do
        ret = ret + i:v;
    end
end
escape ret;
]],
    run = 3,
}

Test { [[
interface I with
    var int v;
end

class Tx with
    interface I;
do
    await FOREVER;
end

pool[] I iss;

var int ret = 0;

spawn Tx with
    this.v = 1;
end;

spawn Tx in iss with
    this.v = 3;
end;

loop i in iss do
    ret = ret + i:v;
end

escape ret;
]],
    run = 3,
}

Test { [[
interface I with
    var int v;
    event void inc;
end

pool[] I iss;

class Tx with
    interface I;
do
    await FOREVER;
end

class U with
    interface I;
do
    await FOREVER;
end

var int ret = 0;
do
    spawn Tx with
        this.v = 1;
    end;
    spawn U in iss with
        this.v = 2;
    end;
    spawn Tx in iss with
        this.v = 3;
    end;

    loop i in iss do
        ret = ret + i:v;
    end
end
escape ret;
]],
    run = 5,
}

Test { [[
interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
end
pool[] Tx ts;

var int ret = 1;
do
    loop i in ts do
        ret = ret + i:v;
    end
end
escape ret;
]],
    run = 1,
}

Test { [[
interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
end
pool[] Tx ts;

var int ret = 1;
do
    spawn Tx in ts with
        this.v = 1;
    end;
    spawn Tx in ts with
        this.v = 2;
    end;
    spawn Tx in ts with
        this.v = 3;
    end;

    loop i in ts do
        ret = ret + i:v;
    end
end
escape ret;
]],
    run = 1,
}

-- TODO: STACK
Test { [[
input void A,B;

interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
    await inc;
    this.v = v + 1;
end

pool[] Tx ts;
var int ret = 1;
do
    spawn Tx in ts with
        this.v = 1;
    end;
    spawn Tx in ts with
        this.v = 2;
    end;
    spawn Tx in ts with
        this.v = 3;
    end;

    loop i in ts do
        ret = ret + i:v;
        watching *i do
            emit i:inc;
            ret = ret + i:v;
        end
    end
end
escape ret;
]],
    props = 'line 28 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 7,
    --run = 13,
}
Test { [[
input void A,B;

interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
    await inc;
    this.v = v + 1;
    await FOREVER;
end

pool[] Tx ts;
var int ret = 1;
do
    spawn Tx in ts with
        this.v = 1;
    end;
    spawn Tx in ts with
        this.v = 2;
    end;
    spawn Tx in ts with
        this.v = 3;
    end;

    loop i in ts do
        ret = ret + i:v;
        watching *i do
            emit i:inc;
            ret = ret + i:v;
        end
    end
end
escape ret;
]],
    props = 'line 29 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 7,
    --run = 13,
}
Test { [[
input void A,B;

interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
    await inc;
    this.v = v + 1;
    await FOREVER;
end
pool[] Tx ts;

var int ret = 0;
do
    par/or do
        await B;
    with
        var int i=1;
        every 1s do
            spawn Tx in ts with
                this.v = i;
                i = i + 1;
            end;
        end
    with
        loop do
            await 1s;
            loop i in ts do
                watching *i do
                    emit i:inc;
                    ret = ret + i:v;
                end
            end
        end
    end
end
escape ret;
]],
    props = 'line 32 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = { ['~>3s;~>B'] = 16 },
    --run = { ['~>3s;~>B'] = 13 },
}

Test { [[
class Tx with
    var int a;
do
end
pool[] Tx ts;

do
    loop t in ts do
        t:a = 1;
    end
end

escape 10;
]],
    run = 10;
}

Test { [[
class Tx with
    event void e;
do
    await FOREVER;
end

event void f;

var Tx t;

par do
    par/or do
        await t.e;
    with
        await 1s;
        emit t.e;
    end
    emit f;
    escape -1;
with
    await f;
    escape 10;
end
]],
    run = { ['~>10s'] = 10 },
}

Test { [[
native do
    int f() { escape 1; }
end
class Tx with do end
spawn Tx with
native _int;
            var& _int? intro_story_str;
native _f;
                do intro_story_str = &_f();
            finalize with
            end
    end;
escape 1;
]],
    gcc = '24: note: expected ‘int *’ but argument is of type ‘int’',
    --run = 1,
}

Test { [[
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v;
    do v = &_getV();
finalize with
    nothing;
end

class Tx with
    var& int v;
do
    v = 20;
end
do Tx with
    this.v = &v;
end;

escape v!;
]],
    env = 'line 21 : invalid operand to unary "&" : cannot be aliased',
}
Test { [[
input int&& SPRITE_DELETE;
class Sprite with
    var& int me;
do
    par/or do
        await SPRITE_DELETE until &&this.me==null;
    with
    end
end
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { [[
par/or do
input int&& SPRITE_DELETE;
var int&& me = await SPRITE_DELETE
               until me == null;
with
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
class Tx with do end
vector[1] Tx ts = [];
escape 1;
]],
    env = 'line 2 : invalid attribution : destination is not a vector',
}
Test { [[
class Tx with do end
pool[10] Tx ts;
escape $$ts;
]],
    env = 'line 3 : invalid operand to unary "$$" : vector expected',
}

Test { [[
class Tx with do end
pool[10] Tx ts;
escape $ts;
]],
    env = 'line 3 : invalid operand to unary "$" : vector expected',
}

Test { [[
class Tx with
    vector&[] int v1;
    vector[] int  v2;
do
    if &&v1==null then end;
end
escape 1;
]],
    run = 1,
    --props = 'line 3 : not permitted inside an interface',
}
Test { [[
class Tx with
    vector[] int  v2;
do
    await FOREVER;
end
var Tx t with
    this.v2 = [1,2,3];
end;
escape t.v2[0]+t.v2[2];
]],
    run = 4,
}

Test { [[
input int&& SDL_KEYDOWN_;
event bool in_tm;

pause/if in_tm do
    class Input with
    do
        await SDL_KEYDOWN_ ;
    end
end

escape 1;
]],
    run = 1,
}

-->>> FUNCTIONS

Test { [[
code/instantaneous Code (var int)=>void
do
end
escape 1;
]],
    --wrn = true,
    --adj = 'line 1 : missing parameter identifier',
    parser = 'line 1 : after `void´ : expected type modifier or `;´',
}

Test { [[
code/instantaneous Code (var int x, var  int)=>void
do
end
escape 1;
]],
    parser = 'line 1 : after `int´ : expected type modifier or internal identifier'
}

Test { [[
code/instantaneous Code (var void, var  int x) => void
do
end
escape 1;
]],
    parser = 'line 1 : after `int´ : expected type modifier or `,´ or `)´',
    --adj = 'line 1 : wrong argument #1 : cannot be `void´',
}

Test { [[
code/instantaneous Code (var void, var  int) => void
do
end
escape 1;
]],
    --wrn = true,
    --adj = 'line 1 : wrong argument #1 : cannot be `void´',
    parser = 'line 1 : after `void´ : expected type modifier or `;´',
}

Test { [[
code/instantaneous Code (var void a, var  int b) => void
do
end
escape 1;
]],
    wrn = true,
    adj = 'line 1 : wrong argument #1 : cannot be `void´',
}

Test { [[
code/instantaneous Code (var int a)=>void
    __ceu_nothing(&&a);
do
end
escape 1;
]],
    parser = 'line 1 : after `void´ : expected type modifier or `;´ or `do´',
}

Test { [[
code/instantaneous Code (var int a)=>void;
code/instantaneous Code (var int a)=>void
do
    native/nohold ___ceu_nothing;
    ___ceu_nothing(&&a);
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Code (var void a)=>void
do
end
escape 1;
]],
    wrn = true,
    adj = 'line 1 : cannot instantiate type "void"',
}

Test { [[
code/instantaneous Code (var void a)=>void
do
end
escape 1;
]],
    wrn = true,
    env = 'TODO: var void',
}

Test { [[
code/instantaneous Code (void)=>void
do
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Code ()=>void
do
end
escape 1;
]],
    parser = 'line 1 : after `(´ : expected `vector´ or `pool´ or `event´ or `var´',
}

Test { [[
code/instantaneous Code (var int x) => int
do
    x = x + 1;
    escape x;
end
var int a = call Code(1);
escape Code(a+10);
]],
    run = 13,
}

Test { [[
code/instantaneous Fx (var int v)=>int do
    escape v+1;
end
escape Fx();
]],
    env = 'line 4 : arity mismatch',
}

Test { [[
code/instantaneous Fx (var int v)=>int do
    escape v+1;
end
var int&& ptr;
escape Fx(ptr);
]],
    env = 'line 5 : wrong argument #1',
}

Test { [[
code/instantaneous Fx (var int v)=>int do
    escape v+1;
end
escape Fx(1);
]],
    run = 2,
}

Test { [[
code/instantaneous Fx (void);
escape 1;
]],
    parser = 'line 1 : after `)´ : expected `=>´',
}

Test { [[
code/instantaneous Fx (void) => void
escape 1;
]],
    parser = 'line 1 : after `void´ : expected type modifier or `;´ or `do´'
}

Test { [[
code/instantaneous Fx (void) => void;
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx void => (void);
escape 1;
]],
    wrn = true,
    parser = 'line 1 : after `Fx´ : expected `(´',
    --parser = 'line 1 : after `Fx´ : expected param list',
    --parser = 'line 1 : after `=>´ : expected type',
}

Test { [[
code/instantaneous Fx (void) => void;
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx (var int) => void do
    escape 1;
end
escape 1;
]],
    --wrn = true,
    --env = 'line 1 : missing parameter identifier',
    parser = 'line 1 : after `void´ : expected type modifier or `;´',
}

Test { [[
code/instantaneous Fx (void) => void do
    event void i;
    emit i;
    await i;
end
escape 1;
]],
    wrn = true,
    props = 'line 3 : not permitted inside `function´',
}

Test { [[
code/instantaneous Fx (void) => void do
    var int a = 1;
    if a then end;
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx (void) => void do
    escape;
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx (void) => void do
    escape 1;
end
escape 1;
]],
    wrn = true,
    --gcc = 'error: ‘escape’ with a value, in function returning void',
    env = 'line 2 : invalid escape value : types mismatch (`void´ <= `int´)',
}

Test { [[
code/instantaneous Fx (void) => void do
    escape;
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
do
    escape 1;
end
escape 1;
]],
    props = 'line 2 : not permitted outside a function',
}

Test { [[
event int a;
a = 1;
escape 1;
]],
    env = 'types mismatch',
}

Test { [[
code/instantaneous Fx (void)=>int do
    escape 1;
end
escape Fx();
]],
    run = 1,
}

Test { [[
code/instantaneous Fx (void)=>int do
    escape 1;
end
escape call Fx();
]],
    todo = 'call?',
    run = 1,
}

Test { [[
code/instantaneous Fx (void) => int;
code/instantaneous Fx (var int x)  => int do end
escape 1;
]],
    env = 'line 2 : function declaration does not match the one at "tests.lua:1"',
    wrn = true,
}

Test { [[
code/instantaneous Fx (void) => int;
code/instantaneous Fx (var int)  => int;
escape 1;
]],
    tops = 'line 2 : identifier "Fx" is already declared',
}

Test { [[
code/instantaneous Fx (void) => int;
code/instantaneous Fx (void) => int do escape 1; end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx (var void, var int) => int;
escape 1;
]],
    wrn = true,
    env = 'line 1 : type cannot be `void´',
}

Test { [[
code/instantaneous Fx (var int) => void;
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx (var int, var int) => int;
code/instantaneous Fx (var int a, var  int b) => int do
    escape a + b;
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx (var int, var int) => int;
code/instantaneous Fx (var int a, var  int b) => int do
    escape a + b;
end
escape Fx(1,2);
]],
    run = 3,
}

Test { [[
code/instantaneous Fff (var int x)=>int do
    escape x + 1;
end

var int x = Fff(10);

input void OS_START;
await OS_START;

escape Fff(x);
]],
    run = 12,
}
Test { [[
output LUA_GETGLOBAL  (var int&&, var byte&&)=>void;
code/instantaneous/recursive Load (var int&& l)=>void do
    loop i do
    end
end
call/recursive Load(null);

escape 1;
]],
    wrn = true,
    tight = 'tight loop',
    --run = 1,
}

Test { [[
native _ceu_out_log;
native do
    ##define ceu_out_call_LUA_GETGLOBAL
end

output LUA_GETGLOBAL  (var int&&, var byte&&)=>void;
code/instantaneous/recursive Load (var int&& l)=>void do
    // TODO: load file
    call LUA_GETGLOBAL => (l, "apps");              // [ apps ]
    call LUA_GETGLOBAL => (l, "apps");              // [ apps ]
    loop i do
        var int has = 1;
        if has==0 then
            break;                                  // [ apps ]
        end
        _ceu_out_log("oi");
    end

    /*
    var int len = (call LUA_OBJLEN => (l, -1));     // [ apps ]
    loop i in [0|>len[ do
        call LUA_RAWGETI => (l, -1);                // [ apps | apps[i] ]
    end
    */
end
call/recursive Load(null);

escape 1;
]],
    tight = 'tight loop',
    run = 1,
}

Test { [[
escape 1;
code/instantaneous Fx (var int x)=>int do
    if x then end;
    loop i in [0 |> 10[ do
    end
    escape 1;
end
]],
    wrn = true,
    run = 1,
}

Test { [[
class Tx with
do
    code/instantaneous Fx (var int)=>int;
    code/instantaneous Fx (var int x)=>int do
        escape x;
    end
    escape Fx(10);
end
var int x = do Tx;
escape x;
]],
    run = {['~>1s']=10},
}

Test { [[
escape 1;
code/instantaneous Fx (void) => int do
    escape 1;
end
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx (void) => void do
    escape 1;
end
escape 1;
]],
    wrn = true,
    props = 'line 2 : not permitted across function declaration',
}

Test { [[
code/instantaneous Set (var u8&& v)=>void do
    *v = 3;
end
var u8 v = 0;
Set(&&v);
escape v;
]],
    run = 3,
}

Test { [[
code/instantaneous Set (var& u8 v)=>void do
    v = 3;
end
var u8 v = 0;
Set(&v);
escape v;
]],
    run = 3,
}

Test { [[
code/instantaneous FillBuffer (vector&[] u8 buf)=>void do
    buf = [] .. buf .. [3];
end
vector[10] u8 buffer;
FillBuffer(&buffer);
escape buffer[0];
]],
    run = 3,
}

Test { [[
code/instantaneous FillBuffer (vector&[20] u8 buf)=>void do
    buf = [] .. buf .. [3];
end
vector[10] u8 buffer;
FillBuffer(&buffer);
escape buffer[0];
]],
    env = 'line 5 : wrong argument #1 : types mismatch (`u8[]&´ <= `u8[]&´) : dimension mismatch',
}

Test { [[
code/instantaneous FillBuffer (vector&[3] u8 buf)=>void do
    buf = [] .. buf .. [2,3,4];
end
vector[3] u8 buffer = [1];
FillBuffer(&buffer);
escape buffer[0];
]],
    run = '2] runtime error: access out of bounds',
}

-- TODO: dropped support for pointers to vectors
Test { [[
code/instantaneous FillBuffer (vector[]&& u8 buf)=>void do
    *buf = [] .. *buf .. [3];
end
vector[10] u8 buffer;
FillBuffer(&&buffer);
escape buffer[0];
]],
    run = 3,
    todo = 'no pointers to vectors',
}

Test { [[
code/instantaneous FillBuffer (vector[3]&& u8 buf)=>void do
    *buf = [] .. *buf .. [2,3,4];
end
vector[3] u8 buffer = [1];
FillBuffer(&&buffer);
escape buffer[0];
]],
    run = '2] runtime error: access out of bounds',
    todo = 'no pointers to vectors',
}

Test { [[
code/instantaneous Build (vector[] u8 bytes)=>void do
end
escape 1;
]],
    wrn = true,
    parser = 'line 1 : after `vector´ : expected `&´',
    --env = 'line 1 : wrong argument #1 : vectors are not supported',
}

Test { [[
code/instantaneous Fx (var int x)=>int do
    escape x + 1;
end

if true then
    escape Fx(1);
else
    escape 0;
end
]],
    run = 2,
}

Test { [[
class Tx with
    var int v = 10;
do
end

var Tx t;

code/instantaneous Fx (void)=>Tx&& do
    escape &&t;
end

var Tx&& p = Fx();

escape p:v;
]],
    run = 10,
}
Test { [[
class Tx with
    var int v = 10;
do
end

var Tx t;

code/instantaneous Fx (void)=>Tx&& do
    var Tx&& p = &&t;
    escape p;
end

var Tx&& p = Fx();

escape p:v;
]],
    run = 10,
}
Test { [[
class Tx with
    var int v = 10;
do
end

var Tx t;

code/instantaneous Fx (void)=>Tx&& do
    var Tx&& p = &&t;
    escape p;
end

var Tx&& p = Fx();
await 1s;

escape p:v;
]],
    fin = 'line 16 : unsafe access to pointer "p" across `await´ (tests.lua : 14)',
}

Test { [[
code/instantaneous Fx (var int x)=>int;
var int x = 0;
code/instantaneous Fx (var int x)=>int do
    this.x = x;
    escape 2;
end
escape Fx(1) + this.x;
]],
    run = 3,
}

Test { [[
code/instantaneous Code (var int)=>void;
code/instantaneous Code (var int a)=>void
do
    escape 1;
end
escape 1;
]],
    wrn = true,
    env = 'TODO: 1 vs void',
    run = 1,
}

--<<< FUNCTIONS

-->>> RECURSIVE / FUNCTIONS

Test { [[
code/instantaneous/recursive Fx (void)=>void;
code/instantaneous/recursive Fx (void)=>void do end
code/instantaneous Gx      (void)=>void;
code/instantaneous/recursive Gx (void)=>void do end
escape 1;
]],
    wrn = true,
    env = 'line 4 : function declaration does not match the one at "tests.lua:3"',
}
Test { [[
code/instantaneous/recursive Fx (void)=>void;
code/instantaneous/recursive Fx (void)=>void do end
code/instantaneous/recursive Gx (void)=>void;
code/instantaneous Gx      (void)=>void do end
escape 1;
]],
    wrn = true,
    env = 'line 4 : function declaration does not match the one at "tests.lua:3"',
}
Test { [[
//var int x;

code/instantaneous/recursive Fa (void)=>void;
code/instantaneous/recursive Fb (void)=>void;

code/instantaneous/recursive Fa (void)=>void do
    if false then
        call/recursive Fb();
    end
end

code/instantaneous/recursive Fb (void)=>void do
    call/recursive Fa();
end

call/recursive Fa();

escape 1;
]],
    run = 1,
}

Test { [[
//var int x;

code/instantaneous Fa (void)=>void;
code/instantaneous Fb (void)=>void;

code/instantaneous Fa (void)=>void do
    Fb();
end

code/instantaneous Fb (void)=>void do
end

Fa();

escape 1;
]],
    run = 1,
}

Test { [[
//var int x;

code/instantaneous Fa (void)=>void;
code/instantaneous Fb (void)=>void;

code/instantaneous Fa (void)=>void do
    Fb();
end

code/instantaneous Fb (void)=>void do
    Fa();
end

Fa();

escape 1;
]],
    tight = 'line 10 : function must be annotated as `@rec´ (recursive)',
}

Test { [[
//var int x;

code/instantaneous Fa (void)=>void;
code/instantaneous/recursive Fb (void)=>void;

code/instantaneous Fa (void)=>void do
    if false then
        call/recursive Fb();
    end
end

code/instantaneous/recursive Fb (void)=>void do
    Fa();
end

Fa();

escape 1;
]],
    tight = 'line 3 : function must be annotated as `@rec´ (recursive)',
}

--<<< RECURSIVE / FUNCTIONS

-->>> METHODS

Test { [[
class Tx with
    var int a;
    code/instantaneous Fx (void)=>int;
do
    var int b;
    code/instantaneous Fx (void)=>int do
        escape b;
    end
    a = 1;
    b = 2;
end

var Tx t;
escape t.a + t.f();
]],
    ref = 'line 7 : invalid access to uninitialized variable "b" (declared at tests.lua:5)',
}
Test { [[
class Tx with
    var int a=0;
    code/instantaneous Fx (void)=>int;
do
    var int b=0;
    code/instantaneous Fx (void)=>int do
        escape b;
    end
    a = 1;
    b = 2;
end

var Tx t;
escape t.a + t.f();
]],
    run = 3,
}

Test { [[
interface I with
    var int v;
    native _f, _a;      // TODO: refuse _a
end
escape 10;
]],
    parser = 'line 2 : after `;´ : expected `var´ or `vector´ or `pool´ or `event´ or `code/instantaneous´ or `code/delayed´ or `interface´ or `input/output´ or `output/input´ or `input´ or `output´ or `end´',
    --run = 10,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    var int x;
    this.x = 10;
    _V = this.x;
end
var Tx t;
escape _V;
]],
    run = 10,
}

Test { [[
class Tx with do end;

code/instantaneous Fff (void)=>void do
    var Tx&& ttt = null;
    if ttt==null then end;
end

do
    var int xxx = 10;
    fff();
    escape xxx;
end
]],
    run = 10,
}

Test { [[
native do
    int V = 0;
end
class Tx with
    code/instantaneous Fx (var int a, var  int b)=>int do
        escape a + b;
    end
do
    _V = _V + f(1,2) + this.f(3,4);
end
vector[2] Tx ts;
escape _V;
]],
    parser = 'line 5 : after `int´ : expected type modifier or `;´',
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    code/instantaneous Fx (var int a, var  int b)=>int do
        escape a + b;
    end
    _V = _V + f(1,2) + this.f(3,4);
end
vector[2] Tx ts;
escape _V;
]],
    run = 20,
}

Test { [[
native do
    int V = 0;
end
class Tx with
do
    var int v=0;
    code/instantaneous Fx (var int a, var  int b)=>void do
        this.v = this.v + a + b;
    end
    f(1,2);
    this.f(3,4);
    _V = _V + v;
end
vector[2] Tx ts;
escape _V;
]],
    run = 20,
}

Test { [[
class Tx with
    var int a=0;
    code/instantaneous Fx (void)=>int;
do
    var int b=0;
    code/instantaneous Fx (void)=>int do
        escape this.b;
    end
    a = 1;
    b = 2;
end

var Tx t;
escape t.a + t.f();
]],
    run = 3,
}
Test { [[
class Tx with
    var int a=0;
    code/instantaneous Fx (void)=>int do
        escape this.b;
    end
do
    var int b;
    a = 1;
    b = 2;
end

var Tx t;
escape t.a + t.f();
]],
    parser = 'line 3 : after `int´ : expected type modifier or `;´',
}

Test { [[
interface I with
    var int v;
    code/instantaneous Fx (void)=>void;
end
escape 10;
]],
    run = 10,
}

Test { [[
class Tx with
    var int v=0;
    code/instantaneous Fx (var int)=>void;
do
    v = 50;
    this.f(10);

    code/instantaneous Fx (var int v)=>int do
        this.v = this.v + v;
        escape this.v;
    end
end

var Tx t;
input void OS_START;
await OS_START;
escape t.v + t.f(20) + t.v;
]],
    wrn = true,
    env = 'line 8 : function declaration does not match the one at "tests.lua:3"',
}

Test { [[
class Tx with
    var int v=0;
    code/instantaneous Fx (var int)=>int;
do
    v = 50;
    this.f(10);

    code/instantaneous Fx (var int v)=>int do
        this.v = this.v + v;
        escape this.v;
    end
end

var Tx t;
input void OS_START;
await OS_START;
escape t.v + t.f(20) + t.v;
]],
    wrn = true,
    run = 220,
}

Test { [[
interface I with
    code/instantaneous Fx (void)=>int;
    code/instantaneous Fa (void)=>int;
end

class Tx with
    interface I;
do
    code/instantaneous Fx (void)=>int do
        escape this.f1();
    end
    code/instantaneous Fa (void)=>int do
        escape 1;
    end
end

var Tx t;
var I&& i = &&t;
escape t.f() + i:f();
]],
    tight = 'line 2 : function must be annotated as `@rec´ (recursive)',
}

Test { [[
interface I with
    code/instantaneous Fa (void)=>int;
    code/instantaneous Fx (void)=>int;
end

class Tx with
    interface I;
do
    code/instantaneous Fa (void)=>int do
        escape 1;
    end
    code/instantaneous Fx (void)=>int do
        escape this.f1();
    end
end

var Tx t;
var I&& i = &&t;
escape t.f() + i:f();
]],
    run = 2,
}

Test { [[
interface I with
    code/instantaneous Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    --run = 120,
    tight = 'line 9 : function must be annotated as `@rec´ (recursive)',
}

Test { [[
interface I with
    code/instantaneous/recursive Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i;
do
    code/instantaneous Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    --run = 120,
    env = 'line 9 : function declaration does not match the one at "tests.lua:2"',
}

Test { [[
interface I with
    code/instantaneous Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    --run = 120,
    env = 'line 9 : function declaration does not match the one at "tests.lua:2"',
}

Test { [[
interface I with
    code/instantaneous Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    --run = 120,
    tight = 'line 9 : function must be annotated as `@rec´ (recursive)',
}

Test { [[
interface I with
    code/instantaneous/recursive Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * (call/recursive i:g(v-1));
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape call/recursive i:g(5);
]],
    run = 120,
}

Test { [[
interface I with
    code/instantaneous/recursive Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    --run = 120,
    tight = 'line 13 : `call/recursive´ is required for "g"',
}

Test { [[
interface I with
    code/instantaneous/recursive Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        escape 1;
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    tight = 'line 9 : function may be declared without `recursive´',
    --tight = 'line 17 : `call/recursive´ is required for "g"',
}
Test { [[
interface I with
    code/instantaneous/recursive Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        escape 1;
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    wrn = true,
    tight = 'line 17 : `call/recursive´ is required for "g"',
}

Test { [[
interface I with
    code/instantaneous/recursive Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        escape v;
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape call/recursive i:g(5);
]],
    --fin = 'line 16 : organism pointer attribution only inside constructors',
    --fin = 'line 16 : attribution to pointer with greater scope',
    --tight = 'line 9 : function may be declared without `recursive´',
    wrn = true,
    run = 5,
}

Test { [[
interface I with
    code/instantaneous/recursive Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        escape v;
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape call/recursive i:g(5);
]],
    --fin = 'line 16 : organism pointer attribution only inside constructors',
    --tight = 'line 9 : function may be declared without `recursive´',
    wrn = true,
    run = 5,
}

Test { [[
interface I with
    code/instantaneous Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous Gx (var int v)=>int do
        escape v;
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    --fin = 'line 16 : organism pointer attribution only inside constructors',
    --fin = 'line 16 : attribution to pointer with greater scope',
    run = 5,
}

Test { [[
interface I with
    code/instantaneous Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous Gx (var int v)=>int do
        escape v;
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    --fin = 'line 16 : organism pointer attribution only inside constructors',
    run = 5,
}

Test { [[
interface I with
    code/instantaneous Gx (var int)=>int;
end

class U with
    interface I;
    var I&& i=null;
do
    code/instantaneous Gx (var int v)=>int do
        escape 1;
    end
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

var Tx t;
var I&& i1 = &&t;
t.i = i1;

var U u;
var I&& i2 = &&u;
t.i = i2;

escape i1:g(5) + i2:g(5);
]],
    --run = 120,
    tight = 'line 18 : function must be annotated as `@rec´ (recursive)',
}

Test { [[
interface I with
    code/instantaneous Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

escape 1;
]],
    --run = 120,
    env = 'line 9 : function declaration does not match the one at "tests.lua:2"',
}

Test { [[
interface I with
    code/instantaneous Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

class U with
    interface I;
    var I&& i=null;
do
    code/instantaneous Gx (var int v)=>int do
        escape 1;
    end
end

var Tx t;
var I&& i1 = &&t;
t.i = i1;

var U u;
var I&& i2 = &&u;
t.i = i2;

escape i1:g(5) + i2:g(5);
]],
    --run = 120,
    tight = 'line 9 : function must be annotated as `@rec´ (recursive)',
}


Test { [[
interface I with
    code/instantaneous/recursive Gx (var int)=>int;
end

class Tx with
    interface I;
    var I&& i=null;
do
    code/instantaneous/recursive Gx (var int v)=>int do
        if (v == 1) then
            escape 1;
        end
        escape v * i:g(v-1);
    end
end

var Tx t;
var I&& i = &&t;
t.i = i;
escape i:g(5);
]],
    tight = 'line 13 : `call/recursive´ is required for "g"',
    --run = 120,
}

Test { [[
pre native do
    typedef int (*f_t) (int v);
end

class Tx with
    var int ret1=0, ret2=0;
    code/instantaneous Fa (var int)=>int;
    var _f_t f2;
do
    native do
        int f2 (int v) {
            escape v;
        }
    end

    code/instantaneous Fa (var int v)=>int do
        escape v;
    end

    ret1 = this.f1(1);
    ret2 = this.f2(2);
end

var Tx t with
    this.f2 = _f2;
end;
escape t.ret1 + t.ret2;
]],
   --fin = 'line 25 : attribution to pointer with greater scope',
    run = 3,
}

Test { [[
pre native do
    typedef int (*f_t) (int v);
end

class Tx with
    var int ret1=0, ret2=0;
    code/instantaneous Fa (var int)=>int;
    var _f_t f2;
do
    native do
        int f2 (int v) {
            escape v;
        }
    end

    code/instantaneous Fa (var int v)=>int do
        escape v;
    end

    ret1 = this.f1(1);
    ret2 = this.f2(2);
end

var Tx t with
    this.f2 = _f2;
end;
escape t.ret1 + t.ret2;
]],
    run = 3,
}

Test { [[
interface I with
    var int v;
    code/instantaneous Ins (void)=>void;
end

class Tx with
    var int v=0;
do
end

var Tx t;
    t.v = 10;
var I&& i = &&t;
escape i:_ins() + t._ins();;
]],
    --env = 'line 14 : native function "CEU_T__ins" is not declared',
    env = 'line 13 : types mismatch (`I&&´ <= `Tx&&´)',
}

Test { [[
interface I with
    var int v;
    code/instantaneous Ins (void)=>int;
end

class Tx with
    interface I;
    var int v=0;
    //native/nohold _ins;
do
    code/instantaneous Ins (void)=>int do
        escape v;
    end
end

var Tx t;
    t.v = 10;
var I&& i = &&t;
escape i:ins() + t.ins();
]],
    run = 20,
}

Test { [[
interface Fx with
    code/instantaneous Fx (void)=>void;
    var int i=10;
end
]],
    env = 'line 3 : invalid attribution',
}

Test { [[
interface Fx with
    var int i;
    code/instantaneous Fx (var int i)=>void;
end

class Tx with
    var int i=10;   // 1
    interface Fx;
do
    this.f(1);
    code/instantaneous Fx (var int i)=>void do
        this.i = this.i + i;
    end
end

var Tx t1;

var Fx&& f = &&t1;
f:f(3);

escape t1.i + f:i;
]],
    wrn = true,
    run = 28,
}

Test { [[
interface Fx with
    var int i;
    code/instantaneous Fx (var int)=>void;
end

class Tx with
    interface Fx;
    var int i=10;   // 2
do
    this.f(1);
    code/instantaneous Fx (var int i)=>void do
        this.i = this.i + i;
    end
end

var Tx t1;

var Fx&& f = &&t1;
f:f(3);

escape t1.i + f:i;
]],
    wrn = true,
    run = 28,
}

Test { [[
native _V;
native do
    void* V;
end
code/instantaneous Fx (var void&& v)=>void do
    _V = v;
end
escape 1;
]],
    wrn = true,
    fin = 'line 5 : attribution to pointer with greater scope',
    --fin = 'line 5 : invalid attribution',
}

Test { [[
native do
    void* V;
end
code/instantaneous Fx (var void&& v)=>void do
    if v then end;
end
escape 1;
]],
    wrn = true,
    -- function can be "@nohold v"
    run = 1,
}

Test { [[
native do
    void* V;
end
class Tx with
    code/instantaneous Fx (var void&& v)=>void;
do
    code/instantaneous Fx (var void&& v)=>void do
native _V;
        _V = v;
    end
end
escape 1;
]],
    wrn = true,
    --fin = 'line 8 : invalid attribution',
    fin = 'line 8 : attribution to pointer with greater scope',
}

Test { [[
class Tx with
    var void&& v=null;
    code/instantaneous Fx (var void&& v)=>void;
do
    code/instantaneous Fx (var void&& v)=>void do
        if v then end;
    end
end
var Tx t;
t.f(null);
escape 1;
]],
    -- function can be "@nohold v"
    wrn = true,
    run = 1,
}

Test { [[
class Tx with
    var void&& a=null;
    code/instantaneous Fx (var void&& v)=>void;
do
    code/instantaneous Fx (var void&& v)=>void do
        var void&& a = v;
        if a then end;
    end
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
class Tx with
    var void&& a=null;
    code/instantaneous Fx (void)=>void;
do
    code/instantaneous Fx (void)=>void do
        var void&& v=null;
        a = v;
    end
end
escape 1;
]],
    -- not from paramter
    fin = 'line 7 : attribution to pointer with greater scope',
}
Test { [[
class Tx with
    var void&& a=null;
    code/instantaneous Fx (var void&& v)=>void;
do
    code/instantaneous Fx (var void&& v)=>void do
        a = v;
    end
end
escape 1;
]],
    -- function must be "@hold v"
    fin = 'line 6 : attribution to pointer with greater scope',
    --fin = ' line 6 : parameter must be `hold´',
}
Test { [[
class Tx with
    var void&& a=null;
    code/instantaneous Fx (var void&& v)=>void;
do
    code/instantaneous Fx (var void&& v)=>void do
        a := v;
    end
end
escape 1;
]],
    -- function must be "@hold v"
    fin = ' line 6 : parameter must be `hold´',
}
Test { [[
class Tx with
    var void&& a=null;
    code/instantaneous Fx (var/hold void&& v)=>void;
do
    code/instantaneous Fx (var/hold void&& v)=>void do
        a := v;
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var void&& v=null;
    code/instantaneous Fx (var void&& v)=>void;
do
    code/instantaneous Fx (var/hold void&& v)=>void do
        this.v = v;
    end
end
escape 1;
]],
    wrn = true,
    env = 'line 5 : function declaration does not match the one at "tests.lua:3"',
}

Test { [[
class Tx with
    var void&& v=null;
    code/instantaneous Fx (var/hold void&& v)=>void;
do
    code/instantaneous Fx (var/hold void&& v)=>void do
        this.v := v;
    end
end
var void&& v=null;
var Tx t;
t.f(null);
t.f(v);
do
    var void&& v=null;
    t.f(v);
end
escape 1;
]],
    wrn = true,
    -- function must be "@hold v" and call must have fin
    fin = 'line 12 : call requires `finalize´',
    --fin = 'line 6 : organism pointer attribution only inside constructors',
}

Test { [[
class Tx with
    var void&& v=null;
    code/instantaneous Fx (var/hold void&& v)=>void;
do
    code/instantaneous Fx (var/hold void&& v)=>void do
        this.v := v;
    end
end
var void&& v=null;
var Tx t;
t.f(null);
t.f(v);
do
    var void&& v=null;
    do t.f(v);
        finalize with
            nothing;
        end;
end
escape 1;
]],
    wrn = true,
    -- function must be "@hold v" and call must have fin
    fin = 'line 12 : call requires `finalize´',
    --fin = 'line 6 : organism pointer attribution only inside constructors',
}

Test { [[
native do
    void* V;
end
code/instantaneous Fx (var void&& v)=>void do
native _V;
    _V := v;
end
var void&& x=null;
Fx(5 as void&&);
escape _V==(5 as void&&);
]],
    fin = 'line 5 : parameter must be `hold´',
    --fin = 'line 5 : invalid attribution',
    --run = 1,
}

Test { [[
native do
    void* V;
end
code/instantaneous Fx (var/hold void&& v)=>void do
native _V;
    _V := v;
end
var void&& x=null;
do Fx(5 as void&&);
    finalize with nothing; end;
escape _V==(5 as void&&);
]],
    fin = 'line 8 : invalid `finalize´',
}

Test { [[
native do
    int V;
end
code/instantaneous Fx (var int v)=>void do
native _V;
    _V = v;
end
var void&& x=null;
Fx(5);
escape _V==5;
]],
    run = 1,
}

Test { [[
interface I with
    code/instantaneous Fx (void)=>void;
end

class Tx with
    interface I;
    code/instantaneous/recursive Fx (void)=>void;
do
    code/instantaneous/recursive Fx (void)=>void do
        if false then
            call/recursive this.Fx();
        end
    end
end

var Tx t;
call/recursive t.Fx();

var I&& i = &&t;
call i:Fx();

escape 1;
]],
    env = 'line 2 : function declaration does not match the one at "tests.lua:7"',
    --tight = 'line 2 : function must be declared with `recursive´',
}

Test { [[
interface I with
    code/instantaneous/recursive Fx (void)=>void;
end

class Tx with
    interface I;
    code/instantaneous/recursive Fx (void)=>void;
do
    code/instantaneous/recursive Fx (void)=>void do
        if false then
            call/recursive this.Fx();
        end
    end
end

var Tx t;
call/recursive t.Fx();

var I&& i = &&t;
call i:Fx();

escape 1;
]],
    tight = 'line 20 : `call/recursive´ is required for "Fx"',
}

Test { [[
interface I with
    code/instantaneous/recursive Fx (void)=>void;
end

class Tx with
    interface I;
    code/instantaneous/recursive Fx (void)=>void;
do
    code/instantaneous/recursive Fx (void)=>void do
        if false then
            call/recursive this.Fx();
        end
    end
end

var Tx t;
call/recursive t.Fx();

var I&& i = &&t;
call/recursive i:Fx();

escape 1;
]],
    run = 1,
}

Test { [[
interface I with
    code/instantaneous/recursive Fx (void)=>void;
end

class Tx with
    interface I;
    code/instantaneous Fx (void)=>void; // ignored
do
    code/instantaneous Fx (void)=>void do
    end
end

var Tx t;
call t.Fx();

var I&& i = &&t;
call/recursive i:Fx();

escape 1;
]],
    env = 'line 2 : function declaration does not match the one at "tests.lua:7"',
}

Test { [[
interface I with
    code/instantaneous Fx (void)=>void;
end

class Tx with
    interface I;
    code/instantaneous Fx (void)=>void; // ignored
do
    code/instantaneous Fx (void)=>void do
    end
end

var Tx t;
call t.Fx();

var I&& i = &&t;
call/recursive i:Fx();

escape 1;
]],
    tight = 'line 17 : `call/recursive´ is not required for "Fx"',
}

Test { [[
interface I with
    code/instantaneous Fx (void)=>void;
end

class Tx with
    interface I;
    code/instantaneous Fx (void)=>void; // ignored
do
    code/instantaneous Fx (void)=>void do
    end
end

var Tx t;
call t.Fx();

var I&& i = &&t;
i:Fx();

escape 1;
]],
    wrn = true,
    --tight = 'line 9 : function may be declared without `recursive´',
    run = 1,
}

Test { [[
interface I with
    code/instantaneous/recursive Fx (void)=>void;
end

class Tx with
    interface I;
    code/instantaneous/recursive Fx (void)=>void; // ignored
do
    code/instantaneous/recursive Fx (void)=>void do
    end
end

var Tx t;
call/recursive t.Fx();

var I&& i = &&t;
call/recursive i:Fx();

escape 1;
]],
    wrn = true,
    --tight = 'line 9 : function may be declared without `recursive´',
    run = 1,
}

Test { [[
interface I with
    code/instantaneous/recursive Fx (void)=>void;
end

class Tx with
    code/instantaneous/recursive Fx (void)=>void;
    interface I;
do
    code/instantaneous/recursive Fx (void)=>void do
    end
end

var Tx t;
call/recursive t.Fx();

var I&& i = &&t;
call/recursive i:Fx();

escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous Fx (var int v)=>int;
code/instantaneous Fx (var int v)=>int do
    if v == 0 then
        escape 1;
    end
    escape v*Fx(v-1);
end
escape Fx(5);
]],
    tight = 'line 2 : function must be annotated as `@rec´ (recursive)',
    --run = 120,
}

Test { [[
interface I with
    code/instantaneous Fx (void)=>void;
end

class Tx with
    interface I;
do
    code/instantaneous Fx (void)=>void do
    end
end

var Tx t;
var& I i = &t;

code/instantaneous Gx (void)=>void do
    i.Fx();
end

code/instantaneous H (void)=>void do
    this.g();
end

escape 1;
]],
    run = 1,
}
Test { [[
interface I with
    code/instantaneous Fx (void)=>void;
end

class Tx with
    interface I;
do
    code/instantaneous Fx (void)=>void do
    end
end

var Tx t;
var& I i = &t;

code/instantaneous Gx (void)=>void do
    i.Fx();
end

code/instantaneous H (void)=>void do
    this.g();
end

class U with
    interface I;
    code/instantaneous Gx (void)=>void;
do
    code/instantaneous Fx (void)=>void do
        this.g();
    end
    code/instantaneous Gx (void)=>void do
    end
end

escape 1;
]],
    tight = 'line 2 : function must be annotated as `@rec´ (recursive)',
}
Test { [[
interface I with
    code/instantaneous/recursive Fx (void)=>void;
end

class Tx with
    interface I;
do
    code/instantaneous/recursive Fx (void)=>void do
    end
end

var Tx t;
var& I i = &t;

code/instantaneous Gx (void)=>void do
    call/recursive i.Fx();
end

code/instantaneous H (void)=>void do
    this.g();
end

class U with
    interface I;
    code/instantaneous Gx (void)=>void;
do
    code/instantaneous/recursive Fx (void)=>void do
        this.g();
    end
    code/instantaneous Gx (void)=>void do
    end
end

escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
code/instantaneous/recursive Fx (var int v)=>int;
code/instantaneous Fx (var int v)=>int do
    if v == 0 then
        escape 1;
    end
    escape v*Fx(v-1);
end
escape Fx(5);
]],
    env = 'line 2 : function declaration does not match the one at "tests.lua:1"',
    --run = 120,
}
Test { [[
code/instantaneous/recursive Fx (var int v)=>int;
code/instantaneous/recursive Fx (var int v)=>int do
    if v == 0 then
        escape 1;
    end
    escape v*Fx(v-1);
end
escape Fx(5);
]],
    tight = 'line 6 : `call/recursive´ is required for "Fx"',
    --run = 120,
}
Test { [[
call 1;
]],
    ast = 'line 1 : invalid call',
    --env = 'TODO: 1 not func',
    --parser = 'line 1 : after `1´ : expected <h,min,s,ms,us>',
}

Test { [[
code/instantaneous/recursive Fx (var int v)=>int;
code/instantaneous/recursive Fx (var int v)=>int do
    if v == 0 then
        escape 1;
    end
    escape v * (call/recursive Fx(v-1));
end
escape Fx(5);
]],
    tight = 'line 8 : `call/recursive´ is required for "Fx"',
}
Test { [[
code/instantaneous/recursive Fx (var int v)=>int;
code/instantaneous/recursive Fx (var int v)=>int do
    if v == 0 then
        escape 1;
    end
    escape v * call/recursive Fx(v-1);
end
escape call/recursive Fx(5);
]],
    run = 120,
}

Test { [[
interface IWorld with
    code/instantaneous Get_pingus (var PinguHolder&&) => PinguHolder&&;
end

class PinguHolder with
do end

class World with
    interface IWorld;
do end

var IWorld&&? ptr = spawn World with end;

escape 1;
]],
    env = 'line 2 : undeclared type `PinguHolder´',
}

Test { [[
interface IWorld with
    code/instantaneous Get_pingus (void) => PinguHolder&&;
end

class PinguHolder with
do end

class World with
    interface IWorld;
do end

var IWorld&&? ptr = spawn World with end;

escape 1;
]],
    env = 'line 2 : undeclared type `PinguHolder´',
}

Test { [[
interface IWorld with
    code/instantaneous Get_pingus (var PinguHolder&&) => void;
end

class PinguHolder with
do end

class World with
    interface IWorld;
do end

var IWorld&&? ptr = spawn World with end;

escape 1;
]],
    env = 'line 2 : undeclared type `PinguHolder´',
}

Test { [[
class PinguHolder with
do end

interface IWorld with
    code/instantaneous Get_pingus (var PinguHolder&&) => PinguHolder&&;
end

class World with
    interface IWorld;
do end

var IWorld&&? ptr = spawn World with end;

escape 1;
]],
    gcc = "undefined reference to `CEU_World_get_pingus'",
}

Test { [[
interface IWorld with
    var int x;
end

class World with
    interface IWorld;
do
    await FOREVER;
end

var IWorld&&? ptr = spawn World with
                    this.x = 10;
                  end;
escape ptr!:x;     // escapes with "10"
]],
    run = 10,
}
Test { [[
interface IWorld with
    var int x;
end

class World with
    interface IWorld;
do
    await FOREVER;
end

var World&&? ptr = spawn World with
                    this.x = 10;
                  end;
var IWorld&& w = ptr!;
escape w:x;     // escapes with "10"
]],
    run = 10,
}

Test { [[
code/instantaneous Fx (void)=>int&& do
    escape 1;
end
escape 10;
]],
    wrn = true,
    env = 'line 2 : invalid escape value : types mismatch (`int&&´ <= `int´)',
}

-- TODO: dropped support for returning alias, is this a problem?

Test { [[
var int x = 10;

code/instantaneous Fx (void)=>int& do
    escape &this.x;
end

escape Fx();
]],
    parser = 'line 3 : after `int´ : expected type modifier or `;´ or `do´',
    --run = 10,
}
Test { [[
class Tx with
    code/instantaneous Fx (void)=>int&;
do
    var int x = 10;
    code/instantaneous Fx (void)=>int& do
        escape &this.x;
    end
end

var Tx t;

escape t.Fx();
]],
    parser = 'line 2 : after `int´ : expected type modifier or `;´',
    --run = 10,
}

Test { [[
var int x = 10;

code/instantaneous Fx (void)=>int& do
    escape &&this.x;
end

escape Fx();
]],
    parser = 'line 3 : after `int´ : expected type modifier or `;´ or `do´',
    --env = 'line 4 : invalid escape value : types mismatch (`int&´ <= `int&&´)',
}
Test { [[
class Tx with
    code/instantaneous Fx (void)=>int&&;
do
    var int x = 10;
    code/instantaneous Fx (void)=>int&& do
        escape &this.x;
    end
end

var Tx t;

escape t.Fx();
]],
    env = 'line 6 : invalid escape value : types mismatch (`int&&´ <= `int&´)',
}

Test { [[
class Test with
    code/instantaneous FillBuffer (vector&[] u8 buf)=>void;
do
    code/instantaneous FillBuffer (vector&[] u8 buf)=>void do
        buf = [] .. buf .. [3];
    end
end

vector[10] u8 buffer;

var Test t;
t.fillBuffer(&buffer);

escape buffer[0];
]],
    run = 3,
}

Test { [[
class Test with
    code/instantaneous FillBuffer (vector[]&& u8 buf)=>void;
do
    code/instantaneous FillBuffer (vector[]&& u8 buf)=>void do
        *buf = [] .. *buf .. [3];
    end
end

vector[10] u8 buffer;

var Test t;
t.fillBuffer(&&buffer);

escape buffer[0];
]],
    --run = 3,
    todo = 'no pointers to vectors',
}

Test { [[
class Tx with
    var int v = 10;
do
end

code/instantaneous Fx (var& Tx t)=>int do
    escape t.v * 2;
end

var Tx t;
var& int ret = Fx(t);

    var Tx u with
        this.v = 20;
    end;
    ret = ret + Fx(&u);

escape ret;
]],
    run = 60,
}

Test { [[
class Tx with
    var int v = 10;
do
end

code/instantaneous Fx (var& Tx t)=>int do
    escape t.v * 2;
end

var Tx t;
var& int ret = Fx(t);

do
    var Tx u with
        this.v = 20;
    end;
    ret = ret + Fx(&u);
end

escape ret;
]],
    --ref = 'line 17 : attribution to reference with greater scope',
    run = 60,
}

Test { [[
class Tx with
    var int v = 10;
do
end

code/instantaneous Fx (var Tx&& t)=>int do
    escape t:v * 2;
end

var Tx t;
var int ret = Fx(&&t);

do
    var Tx u with
        this.v = 20;
    end;
    ret = ret + Fx(&&u);
end

escape ret;
]],
    run = 60,
}

Test { [[
interface Human with
    code/instantaneous Walk (void)=>int;
    code/instantaneous Breath (void)=>int;
    var int n;
end

class CommonThings with
    code/instantaneous Walk (var& Human h)=>int;
    code/instantaneous Breath (var& Human h)=>int;
do
    code/instantaneous Walk (var& Human h)=>int do
        escape h.n;
    end
    code/instantaneous Breath (var& Human h)=>int do
        escape h.n;
    end
    await FOREVER;
end

class Man with
    interface Human;
    var& CommonThings ct;
    var int n = 100;
do
    code/instantaneous Walk (void)=>int do
        escape 200; // override
    end
    code/instantaneous Breath (void)=>int do
        escape this.ct.breath(&this); // delegate
    end
end

var CommonThings ct;
var Man m with
    this.ct = &ct;
end;
escape m.walk() + m.breath();
]],
    run = 300,
}

Test { [[
native do
    typedef struct t {
        void* ceu;
    } t;
end
native/plain _t;
var _t t = _t();
var _t&& ptr = &&t;
var int v = 10;
var int x = &v;
ptr:ceu = &v;
escape *((ptr:ceu as int&&));
]],
    env = 'line 10 : invalid attribution : l-value cannot hold an alias',
    --ref = 'line 9 : invalid attribution : l-value already bounded',
    --run = 10,
}
Test { [[
native do
    typedef struct t {
        void* ceu;
    } t;
end
native/plain _t;
var _t t = _t();
var _t&& ptr = &&t;
var int v = 10;
ptr:ceu = &v;
escape *((ptr:ceu as int&&));
]],
    env = 'line 10 : invalid attribution : l-value cannot hold an alias',
    --ref = 'line 9 : invalid attribution : l-value already bounded',
    --run = 10,
}

Test { [[
native do
    typedef struct t {
        void* xxx;
    } t;
end

class C with
    var int v = 10;
do
end
var C c;

native _t;
var _t   t;
var _t&& ptr = &&t;

ptr:xxx = &c;

escape (ptr:xxx as C&&):v;
]],
    --run = 10,
    env = 'line 16 : invalid attribution : l-value cannot hold an alias',
    --ref = 'line 16 : invalid attribution : l-value already bounded',
}

Test { [[
native do
    typedef struct t {
        void* xxx;
    } t;
end

class C with
    var int v = 10;
    event int e;
do
end
var C c;

native _t;
var _t   t;
var _t&& ptr = &&t;

ptr:xxx = &c;

emit (ptr:xxx as C&&):e => 1;

escape (ptr:xxx as C&&):v;
]],
    env = 'line 17 : invalid attribution : l-value cannot hold an alias',
    --run = 10,
}
Test { [[
class Dir with
    var int value;
do
end
interface IPingu with
    code/instantaneous Get (void)=>Dir&;
end
class Pingu with
    interface IPingu;
do
    var Dir dir with
        this.value = 10;
    end;
    code/instantaneous Get (void)=>Dir& do
        escape &&dir;
    end
end
var Pingu p;
escape p.get().value;
]],
    parser = 'line 6 : after `Dir´ : expected type modifier or `;´',
    --env = 'line 15 : invalid escape value : types mismatch (`Dir&´ <= `Dir&&´)',
}

Test { [[
class Dir with
    var int value;
do
end
interface IPingu with
    code/instantaneous Get (void)=>Dir&;
end
class Pingu with
    interface IPingu;
do
    var Dir dir with
        this.value = 10;
    end;
    code/instantaneous Get (void)=>Dir& do
        escape &dir;
    end
end
var Pingu p;
escape p.get().value;
]],
    parser = 'line 6 : after `Dir´ : expected type modifier or `;´',
    --run = 10,
}

Test { [[
class Tx with do end

pool[] Tx ts;

class U with
    var& int ts;
do
end

var U u;

escape 1;
]],
    ref = 'line 10 : missing initialization for field "ts" (declared in tests.lua:6)',
}

Test { [[
class Tx with do end

pool[] Tx ts;

class U with
    pool&[] Tx ts;
do
    var Tx&&? t =
        spawn Tx in ts with
        end;
end

var U u;

escape 1;
]],
    ref = 'line 13 : missing initialization for field "ts" (declared in tests.lua:6)',
}

Test { [[
interface I with end;

class Tx with
    interface I;
    code/instantaneous Fx (void)=>I&&;
do
    code/instantaneous Fx (void)=>I&& do
        var I&& i = &&this;
        escape i;
    end
end

var Tx t;
var I&& p = t.Fx();
escape p==&&t;
]],
    run = 1,
}

Test { [[
class Tx with
    code/instantaneous Fx (void)=>int&&;
do
    var int x = 1;
    code/instantaneous Fx (void)=>int&& do
        escape &&this.x;
    end
end

var Tx t;
escape *(t.Fx());
]],
    run = 1,
}

Test { [[
interface I with end;

class Tx with
    interface I;
    code/instantaneous Fx (void)=>I&&;
do
    code/instantaneous Fx (void)=>I&& do
        escape &&this;
    end
end

var Tx t;
t.Fx();
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var int xxx2=0;
    code/instantaneous Fff (var int xxx3)=>void;
do
    code/instantaneous Fff (var int xxx3)=>void do
        this.xxx2 = xxx3;
    end
    this.xxx2 = 1;
end

var int xxx1 = 10;
var Tx ttt;
ttt.fff(xxx1);
escape ttt.xxx2;
]],
    run = 10,
}
Test { [[
class Tx with
    var int xxx2=0;
    code/instantaneous Fff (var& int xxx3)=>void;
do
    code/instantaneous Fff (var& int xxx3)=>void do
        var& int xxx4 = &xxx3;
        this.xxx2 = xxx4;
    end
    this.xxx2 = 1;
end

var int xxx1 = 10;
var Tx ttt;
ttt.fff(&xxx1);
escape ttt.xxx2;
]],
    run = 10,
}

Test { [[
class U with do end

class Tx with
    code/instantaneous Fx (var int x)=>Tx;
    var int x = 0;
do
    code/instantaneous Fx (var int x)=>Tx do
        this.x = 1;
    end
end

var Tx t = U.Fx(1);
escape t.x;
]],
    adj = 'line 12 : invalid constructor',
}

Test { [[
class Tx with
    code/instantaneous Fx (var int x)=>Tx;
do
    var int x = 0;

    code/instantaneous Fx (var int x)=>Tx do
        this.x = 1;
    end
end

var Tx t;
t = Tx.Fx(1);
escape t.x;
]],
    parser = 'line 12 : after `Tx´ : expected `(´',
    --parser = 'line 12 : after `.´ : expected tag identifier',
    --parser = 'line 12 : before `.´ : expected expression',
    --run = 2,
}

Test { [[
class Tx with
    code/instantaneous Fff (var int x)=>Tx;
    var int x = 0;
do
    code/instantaneous Fff (var int x)=>Tx do
        this.x = x;
    end
end

var Tx ttt = Tx.fff(2);
escape ttt.x;
]],
    run = 2,
}

Test { [[
class Tx with
    code/instantaneous Fff (var int x)=>Tx;
    var int x = 0;
do
    code/instantaneous Fff (var int x)=>Tx do
        this.x = x;
    end
end

var Tx ttt = Tx.fff(2) with
end;
escape ttt.x;
]],
    run = 2,
}
Test { [[
class Tx with
    code/instantaneous Fff (var int x)=>Tx;
    var int x = 0;
do
    code/instantaneous Fff (var int x)=>Tx do
        this.x = x;
    end
end

var Tx ttt = Tx.fff(2) with
    this.x = 1;
end;
escape ttt.x;
]],
    run = 1,
}

Test { [[
class Tx with
    code/instantaneous Fff (var int x)=>Tx;
    var int x = 0;
do
    code/instantaneous Fff (var int x)=>Tx do
        this.x = x;
    end
    this.x = 1;
end

var Tx ttt = Tx.fff(2);
escape ttt.x;
]],
    run = 1,
}

Test { [[
class Tx with
    code/instantaneous Fa (var int x)=>Tx;
    code/instantaneous Fb (var int x)=>Tx;
    var int x = 0;
do
    code/instantaneous Fa (var int x)=>Tx do
        this.x = x;
    end
    code/instantaneous Fb (var int x)=>Tx do
        this.f1(x);
    end
end

var Tx ttt = Tx.f2(2);
escape ttt.x;
]],
    run = 2,
}

Test { [[
class Tx with
    code/instantaneous Fa (var int x)=>Tx;
    code/instantaneous Fb (var int x)=>Tx;
    var int x = 0;
do
    code/instantaneous Fa (var int x)=>Tx do
        this.x = x;
    end
    code/instantaneous Fb (var int x)=>Tx do
        this.f1(x);
    end
    await FOREVER;
end

pool[] Tx ts;
spawn Tx.f2(2) in ts;

var int ret = 0;
loop t in ts do
    ret = ret + t:x;
end

escape ret;
]],
    run = 2,
}

Test { [[
class Tx with
    code/instantaneous Fa (var int x)=>Tx;
    code/instantaneous Fb (var int x)=>Tx;
    var int x = 0;
do
    code/instantaneous Fa (var int x)=>Tx do
        this.x = x;
    end
    code/instantaneous Fb (var int x)=>Tx do
        this.f1(x);
    end
    escape this.x;
end

var int ret = do Tx.f2(2);
escape ret;
]],
    run = 2,
}

Test { [[
class Tx with
    var& int x;
    code/instantaneous Fff (var& int x)=>Tx;
do
    code/instantaneous Fff (var& int x)=>Tx do
        this.x = x;
    end
    this.x = 1;
end

var int x = 10;
var Tx ttt = Tx.fff(&x);
escape x;
]],
    ref = 'line 6 : invalid attribution : missing alias operator `&´ on the right',
}

Test { [[
class Tx with
    var& int x;
    code/instantaneous Fff (var& int x)=>Tx;
do
    code/instantaneous Fff (var& int x)=>Tx do
    end
end
escape 1;
]],
    ref = 'line 5 : missing initialization for field "x" (declared in tests.lua:2)',
}

Test { [[
class Tx with
    var& int x;
    code/instantaneous Fff (var& int x)=>Tx;
do
    this.x = 1;
    code/instantaneous Fff (var& int x)=>Tx do
    end
end
escape 1;
]],
    ref = 'line 6 : missing initialization for field "x" (declared in tests.lua:2)',
}

Test { [[
class Tx with
    var int xxx2;
    code/instantaneous Fff (var& int xxx3)=>Tx;
do
    code/instantaneous Fff (var& int xxx3)=>Tx do
        this.xxx2 = xxx3;
    end
    this.xxx2 = 1;
end

var int xxx1 = 10;
var Tx ttt = Tx.fff(&xxx1);
escape ttt.xxx2;
]],
    run = 1,
}

Test { [[
class Tx with
    var& int xxx2;
    code/instantaneous Fff (var& int xxx3)=>Tx;
do
    code/instantaneous Fff (var& int xxx3)=>Tx do
        this.xxx2 = &xxx3;
    end
    this.xxx2 = 1;
end

var int xxx1 = 10;
var Tx ttt = Tx.fff(&xxx1);
escape xxx1;
]],
    run = 1,
}

Test { [[
class Tx with
    var& int vvv;
do
end
var Tx t;
escape 1;
]],
    ref = 'line 5 : missing initialization for field "vvv" (declared in tests.lua:2)',
}
Test { [[
var& int vvv;
escape vvv;
]],
    ref = 'line 2 : invalid access to uninitialized variable "vvv" (declared at tests.lua:1)',
}
Test { [[
class TimeDisplay with
    code/instantaneous Build (var& int vvv)=>TimeDisplay;
do
    var int x = 0;
    var& int vvv = &x;

    code/instantaneous Build (var& int vvv)=>TimeDisplay do
        this.vvv = &vvv;
    end
end
escape 1;
]],
    ref = 'line 8 : invalid attribution : variable "vvv" is already bound',
}

Test { [[
class TimeDisplay with
    code/instantaneous Build (var& int vvv)=>TimeDisplay;
do
    var int x = 0;
    var& int vvv;

    code/instantaneous Build (var& int vvv)=>TimeDisplay do
        //this.vvv = &vvv;
        if vvv then end;
    end

    vvv = &x;
    if vvv then end;
end
escape 1;
]],
    run = 1,
}

--<<< METHODS

-->>> CLASS-FINALIZE-OPTION

Test { [[
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v;
    do v = &_getV();
finalize with
    nothing;
end

class Tx with
    var& int? v;
do
    v! = 20;
end
do Tx with
    this.v = &v!;
end;

escape v!;
]],
    run = 20,
}
Test { [[
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v;
    do v = &_getV();
finalize with
    nothing;
end

class Tx with
    var& int? v;
do
    v! = 20;
end
do Tx with
    this.v = &v!;
end;

escape v!;
]],
    run = 20,
}
Test { [[
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v;
    do v = &_getV();
finalize with
    nothing;
end

class Tx with
    var& int v;
do
    v = 20;
end
do Tx with
    this.v = &v;
end;

escape v!;
]],
    env = 'line 21 : invalid operand to unary "&" : cannot be aliased',
}
Test { [[
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v;
    do v = &_getV();
finalize with
    nothing;
end

class Tx with
    var& int v;
do
    v = 20;
end
do Tx with
    this.v = &v!;
end;

escape v!;
]],
    run = 20,
}
Test { [[
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& _int? v;
    do v = &_getV();
finalize with
    nothing;
end

class Tx with
    var& _int v;
do
    v = 20;
end
do Tx with
    this.v = &v;
end;

escape v!;
]],
    env = 'line 21 : invalid operand to unary "&" : cannot be aliased',
}

Test { [[
native _new_Int;
native do
    int* new_Int() {
        escape NULL;
    }
end
    code/instantaneous Parse_file (void) => void do
            var& int? intro_story_str;
            if intro_story_str? then end;
                do intro_story_str = &_new_Int();
            finalize with
                nothing;    /* deleted below */
            end
    end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
native/pure _new_String;
class String with
do
    var& _std__string? ss = &_new_String();
end
escape 1;
]],
    fin = 'line 4 : attribution to pointer with greater scope',
    run = 1,
}

--<<< CLASS-FINALIZE-OPTION

-->>> CLASS-VECTORS-FOR-POINTERS-TO-ORGS

Test { [[
class Tx with
    vector[10] int vs;
do
    this.vs = [1];
end

var Tx t;
t.vs[0] = t.vs[0] + 2;

escape t.vs[0];
]],
    --props = 'line 2 : not permitted inside an interface : vectors',
    run = 3,
}

Test { [[
class Tx with
    vector&[10] int vs;
do
    this.vs = [1];
end

vector[10] int vs;
var Tx t with
    this.vs = &vs;
end;
t.vs[0] = t.vs[0] + 2;

escape t.vs[0];
]],
    run = 3,
}

Test { [[
interface I with
    var& int v;
end

class Tx with
    interface I;
do
    this.v = 1;
end

var int v;
var Tx t with
    this.v = &v;
end;
v = 1;
t.v = t.v + 2;

var I&& i = &&t;
i:v = i:v * 3;

escape t.v;
]],
    ref = 'tests.lua : line 13 : invalid access to uninitialized variable "v" (declared at tests.lua:11)',
    --run = 1,
    --ref = 'line 11 : uninitialized variable "v" crossing compound statement (tests.lua:12)',
}

Test { [[
interface I with
    var& int v;
end

class Tx with
    interface I;
do
    this.v = 1;
end

var int v=10;
var Tx t with
    this.v = &v;
end;
t.v = t.v + 2;

var I&& i = &&t;
i:v = i:v * 3;

escape t.v;
]],
    run = 9,
}

Test { [[
vector&[10] int rs;
vector[10] int  vs = [1];
rs = &vs;
vs[0] = vs[0] + 2;

rs[0] = rs[0] * 3;

escape vs[0];
]],
    run = 9,
}
Test { [[
interface I with
    vector&[10] int vs;
end

class Tx with
    interface I;
do
end

vector[10] int vs;
var Tx t with
    this.vs = &vs;
end;

var I&& i = &&t;

i:vs = [ 0 ];
i:vs[0] = 3;

escape i:vs[0];
]],
    run = 3,
}
Test { [[
interface I with
    vector&[10] int vs;
end

class Tx with
    interface I;
do
end

vector[10] int vs;
var Tx t with
    this.vs = &vs;
end;

var I&& i = &&t;

i:vs[0] = 3;

escape 1;
]],
    run = '17] runtime error: access out of bounds',
    -- TODO: not 20, 17!
}
Test { [[
interface I with
    vector&[10] int vs;
end

class Tx with
    interface I;
do
    this.vs = [1];
end

vector[10] int vs;
var Tx t with
    this.vs = &vs;
end;
t.vs[0] = t.vs[0] + 2;

var I&& i = &&t;

i:vs[0] = i:vs[0] * 3;

escape t.vs[0];
]],
    run = 9,
}
Test { [[
class Tx with
do
end
vector[] Tx&&  ts;
escape 1;
]],
    run = 1,
}
Test { [[
class Tx with
do
end
vector[] Tx&& ts;
var int x = $ts;
escape x+$ts+1;
]],
    run = 1,
}
Test { [[
class Tx with
do
end
vector[] Tx&& ts;
var Tx t;
ts = [] .. ts .. [t];
escape $ts+1;
]],
    env = 'line 6 : wrong argument #1 : types mismatch (`Tx&&´ <= `Tx´)',
}
Test { [[
class Tx with
do
end
vector[] Tx&& ts;
var Tx t;
ts = [] .. ts .. [&&t];
escape $ts+1;
]],
    run = 2,
}

Test { [[
class Tx with
do
end
vector[] Tx&& ts;
var Tx t;
ts = [] .. ts .. [&&t];
var Tx&& p = ts[0];
escape p == &&t;
]],
    run = 1,
}

Test { [[
class Tx with
do
end
vector[] Tx&& ts;
var Tx t;
ts = [] .. ts .. [&&t];
var Tx&& p = ts[1];
escape p == &&t;
]],
    run = '7] runtime error: access out of bounds',
}

Test { [[
class Tx with
do
end
vector[] Tx&& ts;
var Tx t;
ts = [] .. ts .. [&&t];
await 1s;
var Tx&& p = ts[0];
escape p == &&t;
]],
    fin = 'line 8 : unsafe access to pointer "ts" across `await´ (tests.lua : 7)',
}

Test { [[
class Tx with
    var int v = 10;
do
    await FOREVER;
end

var Tx t;
var Tx&&? p = &&t;

escape t.v + p!:v;
]],
    run = 20,
}
Test { [[
class Tx with
    var int v = 10;
do
end

var Tx t;
var Tx&&? p = &&t;

escape t.v + p!:v;
]],
    run = '9] runtime error: invalid tag',
}
Test { [[
class Tx with
    var int v = 10;
do
    await 1s;
end

var Tx t;
var Tx&&? p = &&t;

await 500ms;

escape t.v + p!:v;
]],
    run = { ['~>1s'] = 20 },
}
Test { [[
class Tx with
    var int v = 10;
do
end

var Tx t;
var Tx&&? p = &&t;

await 500ms;

escape t.v + p!:v;
]],
    run = { ['~>1s'] = '12] runtime error: invalid tag', },
}

Test { [[
class Tx with
    var int v = 10;
do
    await 1s;
end

var Tx t;
var Tx&&? ppp = &&t;

await 1s;

escape t.v + ppp!:v;
]],
    run = { ['~>1s']='13] runtime error: invalid tag' },
}

Test { [[
class Tx with
    var int v = 10;
do
    await 1s;
end

var Tx t;
var Tx&&? p = &&t;

await 500ms;

escape t.v + p!:v;
]],
    run = { ['~>1s'] = 20 },
}

Test { [[
class U with
    var int v = 10;
do
    await FOREVER;
end
class Tx with
    var int v = 10;
do
    await 1s;
end

var U u;
var Tx t;
var U&&? p = &&u;

await 1s;

escape t.v + p!:v;
]],
    run = { ['~>1s']=20 },
}

Test { [[
class Tx with
do
end
//vector[] Tx   ts;
vector[] Tx&&  ts1;
vector[] Tx&&? ts2;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
end
vector[] Tx&&? ts;
var Tx t;
ts = [] .. [&&t];
escape $ts;
]],
    run = 1,
}

Test { [[
class Tx with
do
end
vector[] Tx&&? ts;

var Tx t;
ts = [] .. [&&t];

escape ts[0] == &&t;
]],
    env = 'line 9 : invalid operands to binary "=="',
}

Test { [[
class Tx with
do
    await FOREVER;
end
vector[] Tx&&? ts;
var Tx t;
ts = [] .. [&&t];
escape ts[0]! == &&t;
]],
    run = 1,
}

Test { [[
class Tx with
do
end
vector[] Tx&&? ts;
var Tx t;
ts = [] .. [&&t];
escape ts[0]! == &&t;
]],
    run = '7] runtime error: invalid tag',
}

Test { [[
class Tx with
do
end
vector[] Tx&&? ts;
var Tx t;
ts = [] .. [&&t] .. [&&t];
escape ts[1]! == &&t;
]],
    run = '7] runtime error: invalid tag',
}

Test { [[
class Tx with
do
end
vector[] Tx&&? ts;
var Tx t1,t2;
ts = [] .. [&&t1];
ts[0]! = &&t2;
escape ts[0]! == &&t2;
]],
    run = '7] runtime error: invalid tag',
}

Test { [[
class Tx with
do
    await 1s;
end
vector[] Tx&&? ts;
var Tx t;
ts = [] .. [&&t];
await 1s;
escape ts[0]! == &&t;
]],
    run = { ['~>1s']='10] runtime error: invalid tag' },
}

Test { [[
interface I with
end
class Tx with
do
end

var Tx t;
vector[] I&&? iss;
iss = [&&t];

escape iss[0]? + 1;
]],
    run = { ['~>1s'] = 1 },
}

Test { [[
interface I with
end
class Tx with
do
end
class U with
do
    await FOREVER;
end
class V with
do
    await 1s;
end

var Tx t;
var U u;
var V v;

vector[] I&&? iss;
iss = [&&t, &&u, &&v];

var int ret = 0;

ret = ret + iss[0]? + iss[1]? + iss[2]?;
await 1s;
ret = ret + iss[0]? + iss[1]? + iss[2]?;

escape ret;
]],
    run = { ['~>1s'] = 3 },
}

Test { [[
input void OS_START;
class Tx with
do
    native/pure _printf;
    _printf("%p\n", &&this);
    await FOREVER;
end

var Tx&&? t;
par/or do
    do
        pool[] Tx ts;
        t = spawn Tx in ts;
        await OS_START;
    end
    await FOREVER;
with
    await *t!;
end
escape 1;

]],
    _ana = {acc=true},
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
do
    await FOREVER;
end

var Tx&&? t;
par/and do
    do
        pool[] Tx ts;
        t = spawn Tx in ts;
        await OS_START;
    end
with
    await *t!;
end
escape t?==false;
]],
    _ana = {acc=true},
    run = 1,
}

Test { [[
input void OS_START;
class Tx with
do
    await FOREVER;
end

vector[] Tx&&? v;
par/and do
    do
        pool[] Tx ts;
        var Tx&&? ptr = spawn Tx in ts;
        v = [] .. v .. [ptr];
        await OS_START;
    end
with
    await *v[0]!;
end
escape v[0]?==false;
]],
    _ana = {acc=true},
    run = 1,
}

Test { [[
class U with do end;

class Tx with
    vector&[] U&&  us;
    code/instantaneous Build (vector&[] U&& us)=>Tx;
do
    code/instantaneous Build (vector&[] U&& us)=>Tx do
        this.us = &us;
    end
end

vector[] U&& us;
var Tx t = Tx.build(&us);

escape 1;
]],
    run = 1,
}

Test { [[
class U with do end;

class Tx with
    vector&[] U&&  us;
    code/instantaneous Build (vector&[] U&& us)=>Tx;
do
    code/instantaneous Build (vector&[] U&& us)=>Tx do
        this.us = &us;
    end
end

vector[] U&& us = [null];

await 1s;

var& Tx t = Tx.build(us);
var U&& u = us[0];

escape u==null;
]],
    fin = 'line 16 : unsafe access to pointer "us" across `await´ (tests.lua : 14)',
}
Test { [[
class U with do end;

class Tx with
    vector&[] U&&? us;
    code/instantaneous Build (vector&[] U&&? us)=>Tx;
do
    code/instantaneous Build (vector&[] U&&? us)=>Tx do
        this.us = &us;
    end
end

var U u1;
vector[] U&&? us = [&&u1];

await 1s;

var Tx t = Tx.build(&us);
var U&&? u2 = us[0];

escape u2?+1;
]],
    run = { ['~>2s']=1 },
}
Test { [[
class U with
    event bool go;
do
    await FOREVER;
end;

class Tx with
    vector&[] U&&? us;
    code/instantaneous Build (vector&[] U&&? us)=>Tx;
do
    code/instantaneous Build (vector&[] U&&? us)=>Tx do
        this.us = &us;
    end
end

var U u1;
vector[] U&&? us = [&&u1];

await 1s;

var Tx t = Tx.build(&us);
emit us[0]!:go => true;

escape us[0]?+1;
]],
    run = { ['~>2s']=2 },
}
Test { [[
class U with
    event bool go;
do
    await FOREVER;
end;

class Tx with
    vector&[] U&&? us;
    code/instantaneous Build (vector&[] U&&? us)=>Tx;
do
    code/instantaneous Build (vector&[] U&&? us)=>Tx do
        this.us = &us;
    end
end

var U u1;
vector[] U&&? us = [&&u1];
vector&[] U&&? xx = &us;

await 1s;

var Tx t = Tx.build(&us);
emit xx[0]!:go => true;

escape us[0]?+1;
]],
    run = { ['~>2s']=2 },
}

Test { [[
class Tx with
    var& int i;
    code/instantaneous Build (var& int i)=>Tx;
do
    code/instantaneous Build (var& int i)=>Tx do
        this.i = &i;
    end
    escape this.i;
end

var int i = 10;
var int ret = do Tx.build(&this.i);
escape ret;
]],
    run = 10,
}
Test { [[
native do
    ##define ID(x) x
end
native/pure _ID;
class Tx with
    var& int i;
    code/instantaneous Build (var& int i)=>Tx;
do
    code/instantaneous Build (var& int i)=>Tx do
        this.i = &i;
    end
    escape this.i;
end

var int i = 10;
var int ret = do Tx.build(&_ID(&&this.i));
escape ret;
]],
    run = 10,
}

--<<< CLASS-VECTORS-FOR-POINTERS-TO-ORGS

-->>> ISR / ATOMIC

Test { [[
atomic do
    await 1s;
end
escape 1;
]],
    props = 'line 2 : not permitted inside `atomic´',
}

Test { [[
atomic do
    par/or do
        nothing;
    with
        nothing;
    end
end
escape 1;
]],
    props = 'line 2 : not permitted inside `atomic´',
}

Test { [[
output void O;
atomic do
    emit O;
end
escape 1;
]],
    props = 'line 3 : not permitted inside `atomic´',
}

PRE_ISR = [[
pre native do
    tceu_app CEU_APP;
    ##define ceu_out_isr_on()
    ##define ceu_out_isr_off()
    int V;
    void ceu_sys_isr_attach (void* f, int v) {
        V = V + v;
    }
    void ceu_sys_isr_detach (void* f, int v) {
        V = V * v;
    }
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end






]]

Test { PRE_ISR..[[
native do
    void f (void){}
end
atomic do
native _f;
    _f();
end
escape 1;
]],
    run = 1,
}

Test { PRE_ISR..[[
code/instantaneous Fx (void)=>void do end
atomic do
    Fx();
end
escape 1;
]],
    run = 1,
    --props = 'line 4 : not permitted inside `atomic´',
}

Test { [[
atomic do
    loop do
    end
end
escape 1;
]],
    props = 'line 3 : not permitted inside `atomic´',
    wrn = true,
}

Test { [[
loop do
    atomic do
        break;
    end
end
escape 1;
]],
    props = 'line 3 : not permitted inside `atomic´',
}

Test { [[
par/or do
    async/isr [20] do
    end
with
end
escape 1;
]],
    adj = 'line 2 : `async/isr´ must be followed by `await FOREVER´',
}

Test { [[
par/or do
    async/isr [20] do
    end
    await FOREVER;
with
end
escape 1;
]],
    gcc = 'error: implicit declaration of function ‘ceu_out_isr_attach’',
}

Test { [[
native do
    tceu_app CEU_APP;
    void ceu_out_isr_attach (void*) {}
    void ceu_out_isr_detach (void*) {}
end
par/or do
    async/isr [20] do
    end
    await FOREVER;
with
end
escape 1;
]],
    gcc = 'error: #error "Missing definition for macro \\"ceu_out_isr_on\\"."',
}

Test { [[
native do
    tceu_app CEU_APP;
    void ceu_out_isr_attach  (void) { }
    void ceu_out_isr_off (void) { }
end
par/or do
    async/isr [20] do
    end
    await FOREVER;
with
end
escape 1;
]],
    gcc = '5: error: too many arguments to function ‘ceu_out_isr_attach’',
}

Test { [[
pre native do
    tceu_app CEU_APP;
    ##define ceu_out_isr_on
    ##define ceu_out_isr_off
    ##define ceu_out_isr_attach(a,b) __ceu_nothing(a)
    ##define ceu_out_isr_detach(a,b) __ceu_nothing(a)
end
par/or do
    async/isr [1] do
    end
    await FOREVER;
with
end
escape 1;
]],
    run = 1,
}

Test { [[
pre native do
    tceu_app CEU_APP;
    void ceu_sys_isr_attach  (void* f, int v) { }
    void ceu_sys_isr_detach  (void* f, int v, int h) { }
    ##define ceu_out_isr_on
    ##define ceu_out_isr_off
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end
par/or do
    async/isr [1] do
    end
    await FOREVER;
with
end
escape 1;
]],
    gcc = '8:28: error: too few arguments to function ‘ceu_sys_isr_detach’',
}

Test { [[
pre native do
    int V = 1;
    tceu_app CEU_APP;
    ##define ceu_out_isr_on
    ##define ceu_out_isr_off
    void ceu_sys_isr_attach (void* f, int v1, int v2) {
        V = V + v1 + v2;
    }
    void ceu_sys_isr_detach (void* f, int v1, int v2) {
        V = V * v1 - v2;
    }
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end
par/or do
do
    async/isr [3,4] do
    end
    await FOREVER;
end             // TODO: forcing finalize out_isr(null)
with
end
native _V;
escape _V;
]],
    run = 20,
}

Test { [[
pre native do
    int V = 1;
    tceu_app CEU_APP;
    ##define ceu_out_isr_on
    ##define ceu_out_isr_off
    void ceu_sys_isr_attach (void* f, int v) {
        V = V + v;
    }
    void ceu_sys_isr_detach (void* f, int v) {
        V = V * v;
    }
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end
par/or do
    do
        async/isr [3] do
        end
        await FOREVER;
    end             // TODO: forcing finalize out_isr(null)
with
end
native _V;
escape _V;
]],
    run = 12,
}

Test { PRE_ISR..[[
vector[10] int v = [1];
v[0] = 2;
par/or do
    async/isr [20] (v) do
        v[0] = 1;
    end
    await FOREVER;
with
end
escape v[0];
]],
    run = 2,
    --isr = 'line 2 : access to "v" must be atomic',
}

Test { [[
vector[10] int v;
atomic do
    v[0] = 2;
end
par/or do
    async/isr [20] (v) do
        v[0] = 1;
    end
    await FOREVER;
with
end
atomic do
    escape v[0];
end
]],
    props = 'line 13 : not permitted inside `atomic´',
}

Test { [[
native _int;
pre native do
    int V = 1;
    tceu_app CEU_APP;
    ##define ceu_out_isr_on()
    ##define ceu_out_isr_off()
    void ceu_sys_isr_attach (void* f, int v) {
        V = V + v;
    }
    void ceu_sys_isr_detach (void* f, int v) {
        V = V * v;
    }
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end
var _int[10] v = [];
atomic do
    v[0] = 2;
end
par do
    async/isr [20] (v) do
        v[0] = 1;
    end
    await FOREVER;
with
    var int ret;
    atomic do
        ret = v[0];
    end
    escape ret;
end
]],
    _ana = {acc=1},
    run = 2,
}

Test { [[
async/isr [20] do
    atomic do
native _f;
        _f();
    end
end
await FOREVER;
]],
    props = 'line 2 : not permitted inside `async/isr´'
}

Test { [[
pre native do
    int V = 1;
    tceu_app CEU_APP;
    ##define ceu_out_isr_on()
    ##define ceu_out_isr_off()
    void ceu_sys_isr_attach (void* f, int v) {
        V = V + v;
    }
    void ceu_sys_isr_detach (void* f, int v) {
        V = V * v;
    }
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end

interface Global with
    var int x;
end
var int x = 0;

atomic do
    global:x = 1;
end
par/or do
    async/isr [20] do
        global:x = 0;
    end
    await FOREVER;
with
end
escape x;
]],
    run = 1,
}

Test { PRE_ISR..[[
var int v = 2;
par/or do
    async/isr[20] (v) do
        v = 1;
    end
    await FOREVER;
with
end
escape v;
]],
    run = 2,
    --isr = 'line 1 : access to "v" must be atomic',
}

Test { PRE_ISR..[[
var int&& v = null;
par/or do
    async/isr[20] (v) do
        *v = 1;
    end
    await FOREVER;
with
end
escape 1;
]],
    --isr = 'line 4 : pointer access breaks the static check for `atomic´ sections',
    run = 1,
}

Test { PRE_ISR..[[
code/instantaneous Fx (void)=>int do
    escape 2;
end
var int v = Fx();
par/or do
    async/isr [20] do
        Fx();
    end
    await FOREVER;
with
end
escape v;
]],
    --isr = 'line 7 : call breaks the static check for `atomic´ sections',
    run = 2,
}

Test { PRE_ISR..[[
code/instantaneous Fx (void)=>int do
    escape 2;
end
var int v = Fx();
par/or do
    async/isr[20] do
        Fx();
    end
    await FOREVER;
with
end
escape v;
]],
    --wrn = true,
    --isr = 'line 4 : access to "Fx" must be atomic',
    run = 2,
}

Test { PRE_ISR..[[
native do
    int f (void) { escape 2; }
end
native _f;
var int v = _f();
par/or do
    async/isr [20] do
        _f();
    end
    await FOREVER;
with
end
escape v;
]],
    run = 2,
    --wrn = true,
    --isr = 'line 1 : access to "_f" must be atomic',
}

Test { [[
native/pure _f;
pre native do
    int f (void) {
        escape 2;
    }
    ##define ceu_out_isr_on()
    ##define ceu_out_isr_off()
    void ceu_sys_isr_attach (void* f, int v) { }
    void ceu_sys_isr_detach (void* f, int v) { }
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end

var int v = _f();
par/or do
    async/isr [20] do
        _f();
    end
    await FOREVER;
with
end
escape v;
]],
    run = 2,
}

Test { PRE_ISR..[[
var int v;
v = 2;
par/or do
    async/isr [20] (v) do
        v = 1;
    end
    await FOREVER;
with
end
escape v;
]],
    --isr = 'line 2 : access to "v" must be atomic',
    run = 2,
}

Test { PRE_ISR..[[
var int v;
atomic do
    v = 2;
end
par/or do
    async/isr [20] (v) do
        v = 1;
    end
    await FOREVER;
with
end
escape v;
]],
    run = 2,
}

Test { [[
pre native do
    tceu_app CEU_APP;
    ##define ceu_out_isr_on()
    ##define ceu_out_isr_off()
    void ceu_sys_isr_attach (void* f, int v) { }
    void ceu_sys_isr_detach (void* f, int v) { }
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end

var int v;
atomic do
    v = 2;
end
par do
    async/isr [20] (v) do
        v = 1;
        v = 1;
    end
    await FOREVER;
with
    var int ret;
    atomic do
        ret = v;
    end
    escape ret;
end
]],
    _ana = {acc=2},
    run = 2,
}

Test { PRE_ISR..[[
var int v;
atomic do
    v = 2;
end
par/or do
    async/isr [20] do
        this.v = 1;
    end
    await FOREVER;
with
end
escape v;
]],
    --isr = 'line 12 : access to "v" must be atomic',
    props = 'line 27 : not permitted inside `async/isr´',
}

Test { PRE_ISR..[[
var int v;
var int&& p;
atomic do
    v = 2;
    p = &&v;
end
par/or do
    async/isr [20](v) do
        v = 1;
    end
    await FOREVER;
with
end
escape 1;
]],
    --isr = 'line 5 : reference access breaks the static check for `atomic´ sections',
    run = 1,
}

Test { [[
vector[10] int v;
var int&& p;
atomic do
    p = &&v;
end
par/or do
    async/isr [20] do
        this.v[1] = 1;
    end
    await FOREVER;
with
end
escape 1;
]],
    env = 'line 4 : types mismatch (`int&&´ <= `int[]&&´)',
    --env = 'line 4 : invalid operand to unary "&&"',
}

Test { [[
par/or do
    async/isr [1] do
        emit A;
    end
    await FOREVER;
with
end
escape 1;
]],
    tops = 'line 3 : external "A" is not declared',
}

Test { [[
input int A;
par/or do
    async/isr [] do
        emit A;
    end
    await FOREVER;
with
end
escape 1;
]],
    --adj = 'line 3 : missing ISR identifier',
    parser = 'line 3 : after `[´ : expected expression',
}

Test { [[
input int A;
par/or do
    async/isr [1] do
        emit A;
    end
    await FOREVER;
with
end
escape 1;
]],
    env = ' line 4 : arity mismatch',
}

Test { [[
input int A;
pre native do
    tceu_app CEU_APP;
    ##define ceu_out_isr_on()
    ##define ceu_out_isr_off()
    void ceu_sys_isr_attach (void* f, int v) { }
    void ceu_sys_isr_detach (void* f, int v) { }
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
end

par/or do
    async/isr [1] do
        var int x = 111;
        emit A => 1;
        x = 222;
    end
    await FOREVER;
with
end
escape 1;
]],
    run = 1,
}

Test { [[
native _assert;
deterministic _assert;
pre native do
    ##define ceu_out_isr_on()
    ##define ceu_out_isr_off()
    ##define ceu_out_isr_attach ceu_sys_isr_attach
    ##define ceu_out_isr_detach ceu_sys_isr_detach
    int V = 0;
    tceu_app CEU_APP;
    void ceu_out_isr_attach (void* f, int v) {
        V = V + v;
    }
    void ceu_out_isr_detach (void* f, int v) {
        V = V - v;
    }
end

par/or do
native _V;
    _assert(_V==0);
    async/isr [1] do
    end
    await FOREVER;
with
    _assert(_V==1);
    await 1s;
    _assert(_V==1);
end             // TODO: forcing finalize out_isr(null)
_assert(_V==0);
escape _V+1;
]],
    run = { ['~>1s']=1 },
}

Test { [[
native _digitalRead, _digitalWrite;
input int PIN02;
par/or do
    async/isr [1] do
        emit PIN02 => _digitalRead(2);
    end
    await FOREVER;
with
    _digitalWrite(13, 1);
end
escape 1;
]],
    --_ana = {acc=1},
    acc = 'line 8 : access to symbol "_digitalWrite" must be atomic (vs symbol `_digitalRead´ (tests.lua:4))',
    run = 1,
}

Test { [[
input int PIN02;
native _digitalWrite;
par/or do
    var int i = 0;
    async/isr [1] do
        emit PIN02 => i;
    end
    await FOREVER;
with
    _digitalWrite(13, 1);
end
escape 1;
]],
    locs = 'line 6 : internal identifier "i" is not declared',
}

Test { [[
native _digitalWrite;
input int PIN02;
par/or do
    var int i = 0;
    async/isr [1] (i) do
        emit PIN02 => i;
    end
    await FOREVER;
with
    _digitalWrite(13, 1);
end
escape 1;
]],
    gcc = '#error "Missing definition for macro',
}

Test { [[
var int i = 0;
par/or do
    async/isr [1] (i) do
        i = 2;
    end
    await FOREVER;
with
    i = 1;
end
escape 1;
]],
    acc = 'line 9 : access to symbol "i" must be atomic (vs variable/event `i´ (tests.lua:5))',
}

Test { [[
input int PIN02;
var int i = 0;
par/or do
    async/isr [1] (i) do
        i = 2;
    end
    await FOREVER;
with
    atomic do
        i = 1;
    end
end
escape 1;
]],
    _ana = {acc=1},
    gcc = '#error "Missing definition for macro',
}

--<<< ISR / ATOMIC

Test { [[
class U with do end;

pool[10] U  us;

pool[1] U us1;
spawn U in us1;

escape 1;
]],
    wrn = true,
    run = 1,
}

-- TODO: bad message
Test { [[
interface UI with
end

class UIGridItem with
    var UI&&  ui;
do
end

class UIGrid with
    interface UI;
    pool[]  UIGridItem uis;
do
    code/instantaneous Go (void)=>void do
        loop item in this.uis do
native _f;
            _f(item:ui);
        end
    end
end
escape 1;
]],
    wrn = true,
    fin = 'line 15 : unsafe access to pointer "ui" across `class´ (tests.lua : 9)',
}

-- POOLS / 1ST-CLASS

Test { [[
class U with do end;
class Tx with
    pool[0] U us;
do
end

var Tx t;

escape 1;
]],
    run = 1,
}

Test { [[
class U with do end;

interface I with
    pool[10] U us;
end

class Tx with
    interface I;
do
end

var Tx t;
var I&& i = &&t;
spawn U in i:us;

escape 1;
]],
    run = 1,
}

Test { [[
native/plain _int;

interface I with
    var _int[10] vs;
end

interface Global with
    interface I;
end
var _int[10]  vs = [];

class Tx with
    interface I;
do
    global:vs[0] = 1;
end

vs[0] = 1;
global:vs[0] = 1;

var Tx t;
t.vs[0] = 1;

var I&& i = &&t;
i:vs[0] = 1;

escape 1;
]],
    ref = 'line 21 : missing initialization for field "vs" (declared in tests.lua:4)',
}

Test { [[
native/plain _int;

interface I with
    var _int[10] vs;
end

interface Global with
    interface I;
end
var _int[10]  vs = [];

class Tx with
    interface I;
do
    global:vs[0] = 1;
end

vs[0] = 1;
global:vs[0] = 1;

var Tx t with
    this.vs = [];
end;
t.vs[0] = 1;

var I&& i = &&t;
i:vs[0] = 1;

escape 1;
]],
    run = 1,
}

Test { [[
class U with do end;

interface I with
    pool[10] U us;
end

interface Global with
    interface I;
end
pool[10] U  us;

class Tx with
    pool[10] U us;
    interface I;
do
    spawn U in global:us;
end

spawn U in us;
spawn U in global:us;

pool[1] U us1;
spawn U in us1;

var Tx t;
spawn U in t.us;

var I&& i = &&t;
spawn U in i:us;

escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
interface Global with
    var _int[10] vs;
    var int     v;
end
var _int[10] vs = [];
var int     v = 0;

loop i in [0 |> 10[ do
    vs[i] = i;
end
var int ret = 0;
loop i in [0 |> 10[ do
    ret = ret + global:vs[i] + global:v;
end
escape ret;
]],
    run = 45,
}

Test { [[
class Tx with
    var int v = 0;
do
    async do end;
end

interface Global with
    pool[] Tx ts;
end

pool[] Tx ts;

spawn Tx in ts with
    this.v = 10;
end;

var int ret = 0;
loop t in ts do
    ret = ret + t:v;
end

escape ret;
]],
    run = 10,
}

Test { [[
class Tx with
    var int v = 0;
do
    async do end;
end

interface Global with
    pool[1] Tx ts;
end

pool[1] Tx ts;

spawn Tx in ts with
    this.v = 10;
end;

var int ret = 0;
loop t in ts do
    ret = ret + t:v;
end

escape ret;
]],
    run = 10,
}

Test { [[
class Tx with
    var int v = 0;
do
end

interface Global with
    pool[] Tx ts;
end

pool[] Tx ts;

spawn Tx in global:ts;

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var int v = 0;
do
    async do end;
end

interface Global with
    pool[] Tx ts;
end

pool[] Tx ts;

spawn Tx in global:ts with
    this.v = 10;
end;

var int ret = 0;
loop t in global:ts do
    ret = ret + t:v;
end

escape ret;
]],
    run = 10,
}

Test { [[
class Tx with
    var int v = 0;
do
    async do end;
end

interface Global with
    pool[] Tx ts;
end

pool[] Tx ts;

spawn Tx in global:ts with
    this.v = 10;
end;

var int ret = 0;
loop t in global:ts do
    ret = ret + t:v;
end

escape ret;
]],
    run = 10,
}

Test { [[
class Tx with
do
end

interface Global with
    pool[] Tx ts;
end

pool[] Tx ts;

class U with
    var int v = 0;
do
    spawn Tx in global:ts with
    end;
end

var U u;
escape 1;
]],
    run = 1,
}
Test { [[
class Tx with
    var int v = 0;
do
    async do end;
end

interface Global with
    pool[] Tx ts;
end

pool[] Tx ts;

class U with
    var int v = 0;
do
    spawn Tx in global:ts with
        this.v = 10;
    end;
    spawn Tx in global:ts with
        this.v = 20;
    end;

    loop t in global:ts do
        this.v = this.v + 10;
    end
end

var int ret = 0;

do
    var U u;
    ret = ret + u.v;
end

loop t in global:ts do
    ret = ret + t:v;
end

escape ret;
]],
    run = 50,
}

Test { [[
class Tx with
    var int v = 0;
do
    async do end;
end

interface Global with
    pool[1] Tx ts;
end

pool[1] Tx ts;

class U with
    var int v = 0;
do
    spawn Tx in global:ts with
        this.v = 10;
    end;
    spawn Tx in global:ts with
        this.v = 20;
    end;

    loop t in global:ts do
        this.v = this.v + 10;
    end
end

var int ret = 0;

do
    var U u;
    ret = ret + u.v;
end

loop t in global:ts do
    ret = ret + t:v;
end

escape ret;
]],
    run = 20,
}

Test { [[
native do
    int V = 0;
end

class Tx with
    var int v = 0;
do
    do finalize with
native _V;
        _V = _V + 1;
    end
    await FOREVER;
end

class U with
    var int v = 0;
    pool[1] Tx ts;
do
    await FOREVER;
end

var int ret = 0;

do
    var U u;
    spawn Tx in u.ts with
        this.v = 10;
    end;
    spawn Tx in u.ts with
        this.v = 20;
    end;

    loop t in u.ts do
        ret = ret + t:v;
    end
end

async do end;

escape ret + _V;
]],
    run = 11,
}

Test { [[
native do
    int V = 0;
end

class Tx with
    var int v = 0;
do
    do finalize with
native _V;
        _V = _V + 1;
    end
    await FOREVER;
end

class U with
    var int v = 0;
    pool[] Tx ts;
do
    await FOREVER;
end

var int ret = 0;

do
    var U u;
    spawn Tx in u.ts with
        this.v = 10;
    end;
    spawn Tx in u.ts with
        this.v = 20;
    end;

    loop t in u.ts do
        ret = ret + t:v;
    end
end

async do end;

escape ret + _V;
]],
    run = 32,
}

Test { [[
class Unit with
do
    spawn Unit in global:units;
end
interface Global with
    pool[] Unit units;
end
pool[] Unit units;
escape 1;
]],
    env = 'line 3 : interface "Global" is not defined',
    --env = 'line 3 : undeclared type `Unit´',
}
Test { [[
interface Global with
    pool[] Unit units;
end
class Unit with
do
    spawn Unit in global:units;
end
pool[] Unit units;
escape 1;
]],
    env = 'line 2 : undeclared type `Unit´',
}
Test { [[
interface U with end;

interface Global with
    pool[] U units;
end
native/nohold _SDL_Has;

class V with
    interface U;
do
end

class Unit with
    interface U;
    var int rect;
do
    loop oth in global:units do
        if oth!=&&this then
            spawn V in global:units;
        end
    end
end

pool[] U units;
escape 1;
]],
    props = 'line 17 : pool iterator cannot contain yielding statements (`await´, `emit´, `spawn´, `kill´)',
    --run = 1,
}

-- declaration order for clss, ifcs, pools

Test { [[
    class Queue with
      pool[] QueueForever val;
    do
      //
    end
    escape 1;
]],
    env = 'line 2 : undeclared type `QueueForever´',
}
Test { [[
    var Queue q;
    class Queue with
    do
        var Queue q;
    end
    escape 1;
]],
    env = 'line 1 : undeclared type `Queue´',
}
Test { [[
    class Queue with
    do
        var Queue q;
    end
    var Queue q;
    escape 1;
]],
    env = 'line 3 : undeclared type `Queue´',
}
Test { [[
    class Queue with
    do
        var Queue&& q=null;
        if q then end;
    end
    var Queue q;
    escape 1;
]],
    run = 1,
}
Test { [[
    class Queue with
      pool[] QueueForever val;
    do
    end

    class QueueForever with
    do
    end

    escape 1;
]],
    env = 'line 2 : undeclared type `QueueForever´',
}
Test { [[
    interface I with
      var int val;
    end
    spawn I;
    escape 1;
]],
    env = 'line 4 : cannot instantiate an interface',
}
Test { [[
    class QueueForever with
      var int val=0, maxval=0;
    do
        spawn QueueForever;
    end
    escape 1;
]],
    wrn = 'line 4 : unbounded recursive spawn',
    run = 1,
    --env = 'line 4 : undeclared type `QueueForever´',
}
Test { [[
    class Queue with
      pool[] QueueForever val;
    do
      //
    end

    class QueueForever with
      var& Queue queue;
      var int val, maxval;
    do
      if val < maxval then
        spawn QueueForever in queue.val with
          this.queue = outer.queue;
          this.val = outer.val + 1;
          this.maxval = outer.maxval;
        end;
      end
    end

    var Queue queue;

    watching 1000us do
      spawn QueueForever in queue.val with
        this.queue = queue;
        this.val = 0;
        this.maxval = 1000;
      end;
    end
    escape 0;
]],
    env = 'line 2 : undeclared type `QueueForever´',
}

Test { [[escape(1);]],
    _ana = {
        isForever = false,
    },
    run = 1,
}

-- test case for bad stack clear
Test { [[
class Intro with
do
    await 20ms;
end

do Intro;

class Body with do
    await 10ms;
end

class BTreeTraverse with
do
    pool[0] Body bodies;
    do Body;
    await 10ms;
end

do BTreeTraverse;

escape 1;
]],
    run = {['~>1s']=1},
}

-- TRACKING / WATCHING

Test { [[
var int n =
    watching 1s do
        await FOREVER;
    end;
escape n;
]],
    run = { ['~>1001ms'] = 1000 },
}

Test { [[
event int e;
par do
    var int n =
        watching e do
            await FOREVER;
        end;
    escape n;
with
    await 1s;
    emit e => 10;
    await FOREVER;
end
]],
    run = { ['~>1001ms'] = 10 },
}

Test { [[
class Tx with
    event int e;
do
    await 1s;
    escape 10;
end

var Tx t;
var int n =
    watching t do
        await FOREVER;
    end;

escape n;
]],
    run = { ['~>1001ms'] = 10 },
}

Test { [[
var int n =
    watching 1s do
        escape 1;
    end;
escape n;
]],
    run = { ['~>1001ms'] = 1 },
}

Test { [[
input void E;
event int e;
par do
    var int n =
        watching e do
            await 300ms;
            escape 1;
        end;
    escape n;
with
    await E;
    emit e => 10;
    await FOREVER;
end
]],
    run = { ['~>1001ms'] = 1 },
}

Test { [[
event int e;
par do
    var int n =
        watching e do
            await 300ms;
            escape 1;
        end;
    escape n;
with
    await 1s;
    emit e => 10;
    await FOREVER;
end
]],
    _ana = { acc=1 },
    run = { ['~>1001ms'] = 1 },
}

Test { [[
input void E;
class Tx with
    event int e;
do
    await E;
    escape 10;
end

var Tx t;
var int n =
    watching t do
        await 500ms;
        escape 1;
    end;

escape n;
]],
    run = { ['~>1001ms'] = 1 },
}

Test { [[
class Tx with
    event void e;
do
    await this.e;
    watching this.e do
        nothing;
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    event int e;
do
    await this.e;
    var int v;
    watching this.e do
        if v then end;
        nothing;
    end
end
escape 1;
]],
    ref = 'line 7 : invalid access to uninitialized variable "v" (declared at tests.lua:5)',
}

Test { [[
class Tx with
    event int e;
do
    await this.e;
    var int v =
        watching this.e do
            if v then end;
            nothing;
        end;
end
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
do
end

var Tx t;

par/or do
every 1s do
    watching t do
    end
end
with
end

escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    event void e;
do
    await this.e;
    par/or do
        nothing;
    with
        if true then
            await this.e;
        else
        end
    end
    watching this.e do
        nothing;
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
input int I;
var int ret = -5;
watching I do
    await 1s;
    ret = 5;
end
escape ret;
]],
    run = {
        ['100~>I; ~>1s'] = -5,
        ['~>1s; 100~>I'] = 5,
    }
}

Test { [[
input int I;
var int ret = -5;
var int v=
watching I do
    await 1s;
    ret = 5;
end;
escape ret+v;
]],
    run = {
        ['100~>I; ~>1s'] = 95,
        ['~>1s; 100~>I'] = 5,
    }
}

Test { [[
watching (10)ms do
end
escape 1;
]],
    run = {
        ['~>1s'] = 1,
    }
}

Test { [[
input int I;
var int ret = -5;
var int dt = await I;
watching (dt)ms do
    await 1s;
    ret = 5;
end
escape ret;
]],
    run = {
        ['100~>I; ~>1s'] = -5,
        ['1000~>I; ~>1s'] = -5,
        ['1001~>I; ~>1s'] = 5,
    }
}

Test { [[
input int I;
var int ret = -5;
event void e;
par/or do
    loop do
        var int dt = await I;
        if dt == 100 then
            emit e;
        end
    end
with
    watching e do
        await 1s;
        ret = 5;
    end
end
escape ret;
]],
    run = {
        ['100~>I; ~>1s'] = -5,
        ['1000~>I; ~>1s'] = 5,
    }
}

-- TODO: "e" has type "Tx*"
-- this type is defined inside the application and only makes sense there
-- if it is not the case, simply use void* and the other application casts back 
-- to Tx*
Test { [[
data Tx;
event Tx&& e;
var int ret = -1;
watching e do
    await 1s;
    ret = 1;
end
escape 1;
]],
    env = 'line 1 : invalid event type',
    --gcc = ' error: unknown type name ‘Tx’',
    --run = { ['~>1s'] = 1 }
}

Test { [[
class U with
    var int v = 0;
do
    await FOREVER;
end;

interface I with
    pool[2] U us2;
end

class Tx with
    pool[2] U us1;
    interface I;
do
end

var Tx t;
spawn U in t.us2 with
    this.v = 1;
end;

var I&& i = &&t;
spawn U in i:us2 with
    this.v = 2;
end;

var int ret = 0;

loop u in t.us2 do
    ret = ret + u:v;
end

loop u in i:us2 do
    ret = ret + u:v;
end

escape ret;
]],
    fin = 'line 33 : unsafe access to pointer "i" across `spawn´',
}

Test { [[
class U with
    var int v = 0;
do
    await FOREVER;
end;

interface I with
    pool[2] U us2;
end

class Tx with
    pool[2] U us1;
    interface I;
do
    await FOREVER;
end

var Tx t;
spawn U in t.us2 with
    this.v = 1;
end;

var I&& i = &&t;

var int ret = 1;

watching *i do
    spawn U in i:us2 with
        this.v = 2;
    end;

    loop u in t.us2 do
        ret = ret + u:v;
    end

    loop u in i:us2 do
        ret = ret + u:v;
    end
end

escape ret;
]],
    run = 7,
}

Test { [[
class U with
    var int v = 0;
do
    await FOREVER;
end;

interface I with
    pool[2] U us2;
end

class Tx with
    pool[2] U us1;
    interface I;
do
end

var Tx t;
spawn U in t.us2 with
    this.v = 1;
end;

var I&& i = &&t;

var int ret = 1;

watching *i do
    spawn U in i:us2 with
        this.v = 2;
    end;

    loop u in t.us2 do
        ret = ret + u:v;
    end

    loop u in i:us2 do
        ret = ret + u:v;
    end
end

escape ret;
]],
    run = 1,
}

Test { [[
class Tx with
    var int v = 0;
do
end

event Tx&& e;
var int ret = 0;

par/or do
    var Tx t with
        this.v = 10;
    end;
    async do end;
    emit e => &&t;
with
    var Tx&& p = await e;
    ret = p:v;
end

escape ret;
]],
    env = 'line 6 : invalid event type',
    --env = 'line 14 : wrong argument : cannot pass pointers',
    --run = 10,
}

Test { [[
class Tx with
    var int v = 0;
do
end

event Tx&& e;
var int ret = 0;

par/or do
    var Tx t with
        this.v = 10;
    end;
    async do end;
    emit e => &&t;
with
    var Tx&& p = await e;
    ret = p:v;
end

escape ret;
]],
    env = 'line 6 : invalid event type',
    --env = 'line 14 : wrong argument : cannot pass pointers',
    --run = 10,
    safety = 2,
}

Test { [[
class Tx with
    var int v = 0;
do
end

event Tx&& e;
var int ret = 0;

par/or do
    var Tx t with
        this.v = 10;
    end;
    async do end;
    emit e => &&t;
with
    var Tx&& p = await e;
    async do end;
    ret = p:v;
end

escape ret;
]],
    env = 'line 6 : invalid event type',
    --env = 'line 14 : wrong argument : cannot pass pointers',
    --fin = 'line 18 : unsafe access to pointer "p" across `async´'
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var int v = 0;
do
end

event Tx&& e;
var int ret = 0;

par/or do
    var Tx t with
        this.v = 10;
    end;
    async do end;
    emit e => &&t;
with
    var I&& p = await e;
    async do end;
    ret = p:v;
end

escape ret;
]],
    env = 'line 10 : invalid event type',
    --env = 'line 18 : wrong argument : cannot pass pointers',
    --fin = 'line 22 : unsafe access to pointer "p" across `async´',
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var int v = 0;
do
    await FOREVER;
end

var I&&? p = spawn Tx with
    this.v = 10;
end;
escape p!:v;
]],
    run = 10,
    --fin = 'line 22 : invalid access to awoken pointer "p"',
}

Test { [[
native do
    int V = 0;
end
input void OS_START;
class Tx with
    var int id = 0;
do
    await OS_START;
native _V;
    _V = _V + 1;
end

pool[1] Tx ts;
var Tx&&? t = spawn Tx in ts with
    this.id = 10;
end;

var int ret = 0;
watching *t! do
    ret = t!:id;
    await FOREVER;
end

escape ret;
]],
    _ana = { acc=true },
    run = 10,
}

Test { [[
input void OS_START;
class Tx with
    var int id = 0;
do
    await OS_START;
end

pool[1] Tx ts;
var Tx&&? t = spawn Tx in ts with
    this.id = 10000;
end;

var int ret = 0;

watching *t! do
    ret = t!:id;
    await FOREVER;
end

escape ret;
]],
    run = 10000,
}
Test { [[
input void OS_START;
class Tx with
    var int id = 0;
do
    await OS_START;
end

pool[2] Tx ts;
var Tx&&? t1 = spawn Tx in ts with
    this.id = 10000;
end;
var Tx&&? t = spawn Tx in ts with
    this.id = 10000;
end;

var int ret = 0;

watching *t! do
    ret = t!:id;
    await FOREVER;
end

escape ret;
]],
    run = 10000,
}
Test { [[
native do
    int V = 0;
end
input void OS_START;
class Tx with
    var int id = 0;
do
    await OS_START;
native _V;
    _V = _V + 1;
end

pool[10000] Tx ts;
var Tx&& t0 = null;
var Tx&& tF = null;
loop i in [0 |> 10000[ do
    var Tx&&? t = spawn Tx in ts with
        this.id = 10000-i;
    end;
    if t0 == null then
        t0 = t!;
    end
    tF = t!;
end
native _assert;
_assert(t0!=null and tF!=null);

var int ret1=0, ret2=0;

watching *tF do
    ret2 = tF:id;
    await FOREVER;
end

escape ret1+ret2+_V;
]],
    --run = 10001,
    --fin = 'line 19 : unsafe access to pointer "t0" across `spawn´',
    fin = 'line 19 : unsafe access to pointer "t0" across `loop´',
}

Test { [[
native do
    int V = 0;
end
input void OS_START;
class Tx with
    var int id = 0;
do
    await OS_START;
native _V;
    _V = _V + 1;
end

pool[10000] Tx ts;
var Tx&& tF = null;
loop i in [0 |> 10000[ do
var Tx&& t0 = null;
    var Tx&&? t = spawn Tx in ts with
        this.id = 10000-i;
    end;
    if t0 == null then
        t0 = t!;
    end
    tF = t!;
end
native _assert;
_assert(tF!=null);

var int ret1=0, ret2=0;

watching *tF do
    ret2 = tF:id;
    await FOREVER;
end

escape ret1+ret2+_V;
]],
    --run = 10001,
    fin = 'line 19 : unsafe access to pointer "t0" across `spawn´',
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var int v = 10;
do
    await FOREVER;
end

var I&&? p = spawn Tx;
escape p!:v;
]],
    run = 10,
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var int v = 0;
do
end

var I&&? p = spawn Tx with
    p!:v = 10;
end;
async do end;

escape p!:v;
]],
    run = '15] runtime error: invalid tag',
    --run = 1,
    --fin = 'line 15 : unsafe access to pointer "p" across `async´',
}

Test { [[
class Unit with
    event int move;
do
end
var Unit&&? u;
do
    pool[] Unit units;
    u = spawn Unit in units;
end
if u? then
    watching *u! do
        emit u!:move => 0;
    end
end
escape 2;
]],
    run = 2,
}
Test { [[
class Unit with
    event int move;
do
end
var Unit&&? u;
do
    pool[] Unit units;
    u = spawn Unit in units;
    await 1min;
end
watching *u! do
    emit u!:move => 0;
end
escape 2;
]],
    run = { ['~>1min']='12] runtime error: invalid tag', },
    --fin = 'line 11 : unsafe access to pointer "u" across `await´',
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var I&& i = null;
do
    watching *i do
        var int v = i:v;
        if v then end
    end
end

escape 1;
]],
    run = 1,
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var I&& i = null;
do
    await 1s;
    watching *i do
        var int v = i:v;
    end
end

escape 1;
]],
    fin = 'line 9 : unsafe access to pointer "i" across `await´',
}

Test { [[
interface I with
    var int v;
end

class Tx with
    var I&& i = null;
do
    watching *i do
        await 1s;
        var int v = i:v;
        if v then end
    end
end

escape 1;
]],
    run = 1,
}

Test { [[
interface I with
    var int v;
    event void e;
end

var I&& i=null;

await 1s;

await i:e;

watching *i do
    await 1s;
    var int v = i:v;
end

escape 1;
]],
    fin = 'line 10 : unsafe access to pointer "i" across `await´',
}
Test { [[
interface I with
    var int v;
    event void e;
end

var I&& i=null;

await 1s;

watching *i do
    await 1s;
    var int v = i:v;
end

escape 1;
]],
    fin = 'line 10 : unsafe access to pointer "i" across `await´',
}

Test { [[
interface I with
    var int v;
end

var I&& i=null;

par/or do
    watching *i do
        await 1s;
        var int v = i:v;
    end
with
    await 1s;
end

escape 1;
]],
    todo = '*i vai dar segfault',
    run = 1,
}

Test { [[
class Tx with
do
end
var Tx t;
watching t do
end
escape 100;
]],
    run = 100,
}

Test { [[
class Tx with
    var int v = 0;
do
end

event Tx&& e;
var int ret = 1;

par/and do
    async do end;
    var Tx t with
        this.v = 10;
    end;
    emit e => &&t;
    await 1s;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 1 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 6 : invalid event type',
    --env = 'line 14 : wrong argument : cannot pass pointers',
    --run = { ['~>5s']=1 },
}

Test { [[
class Tx with
    var int v = 0;
do
end

event Tx&& e;
var int ret = 1;

par/and do
    async do end;
    var Tx t with
        this.v = 10;
    end;
    emit e => &&t;
    await 1s;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 1 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 6 : invalid event type',
    --env = 'line 14 : wrong argument : cannot pass pointers',
    --run = { ['~>5s']=1 },
    safety = 2,
}

Test { [[
class Tx with
    var int v = 0;
do
end

event Tx&& e;
var int ret = 1;

par/and do
    async do end;
    var Tx t with
        this.v = 10;
    end;
    emit e => &&t;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 1 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 6 : invalid event type',
    --env = 'line 14 : wrong argument : cannot pass pointers',
    --run = { ['~>5s']=1 },
}

Test { [[
class Tx with
    var int v = 0;
do
    await 5s;
end

event Tx&& e;
var int ret = 0;

par/and do
    async do end;
    var Tx t with
        this.v = 10;
    end;
    emit e => &&t;
    await 6s;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 0 then
                ret = -1;
            end
        end
        await 4s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 7 : invalid event type',
    --env = 'line 15 : wrong argument : cannot pass pointers',
    --run = { ['~>10s']=10 },
}

Test { [[
class Tx with
    var int v = 0;
do
    await 4s;
end

event Tx&& e;
var int ret = 0;

par/and do
    async do end;
    var Tx t with
        this.v = 10;
    end;
    emit e => &&t;
    await 6s;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 0 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 7 : invalid event type',
    --env = 'line 15 : wrong argument : cannot pass pointers',
    --run = { ['~>10s']=-1 },
}

Test { [[
class Tx with
    var int v = 0;
do
    await 6s;
end

event Tx&& e;
var int ret = 0;

par/and do
    async do end;
    var Tx t with
        this.v = 10;
    end;
    emit e => &&t;
    await 6s;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 0 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 7 : invalid event type',
    --env = 'line 15 : wrong argument : cannot pass pointers',
    --run = { ['~>10s']=10 },
}

Test { [[
class Tx with
    var int v = 0;
do
end

event Tx&& e;
emit e => null;
escape 1;
]],
    env = 'line 6 : invalid event type',
    --env = 'line 7 : wrong argument : cannot pass pointers',
    --run = 1;
}

Test { [[
class Tx with
    var int v = 0;
do
    async do end
end

event Tx&& e;
var int ret = 1;

par/and do
    async do end;
    pool[] Tx ts;
    var Tx&&? t = spawn Tx in ts with
        this.v = 10;
    end;
    emit e => t!;
    await 1s;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 1 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 7 : invalid event type',
    --env = 'line 16 : wrong argument : cannot pass pointers',
    --run = { ['~>1s;~>1s;~>1s;~>1s;~>1s']=-1 },
}

Test { [[
class Tx with
    var int v = 0;
do
    async do end
end

event Tx&& e;
var int ret = 1;

par/and do
    async do end;
    pool[] Tx ts;
    var Tx&&? t = spawn Tx in ts with
        this.v = 10;
    end;
    emit e => t!;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 0 then
                ret = 1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 7 : invalid event type',
    --env = 'line 16 : wrong argument : cannot pass pointers',
    --run = { ['~>1s;~>1s;~>1s;~>1s;~>1s']=1 },
}

Test { [[
class Tx with
    var int v = 0;
do
    await 4s;
end

event Tx&& e;
var int ret = 0;

par/and do
    async do end;
    pool[] Tx ts;
    var Tx&&? t = spawn Tx in ts with
        this.v = 10;
    end;
    emit e => t!;
    await 6s;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 0 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 7 : invalid event type',
    --env = 'line 16 : wrong argument : cannot pass pointers',
    --run = { ['~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s']=-1 },
}

Test { [[
class Tx with
    var int v = 0;
do
    await 6s;
end

event Tx&& e;
var int ret = 0;

par/and do
    async do end;
    pool[] Tx ts;
    var Tx&&? t = spawn Tx in ts with
        this.v = 10;
    end;
    emit e => t!;
    await 6s;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 0 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 7 : invalid event type',
    --env = 'line 16 : wrong argument : cannot pass pointers',
    --run = { ['~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s']=10 },
}

Test { [[
class Tx with
    var int v = 0;
do
    await 6s;
end

event Tx&& e;
var int ret = 0;

par/and do
    async do end;
    pool[] Tx ts;
    var Tx&&? t = spawn Tx in ts with
        this.v = 10;
    end;
    emit e => t!;
with
    var Tx&& p = await e;
    watching *p do
        do finalize with
            if ret == 0 then
                ret = -1;
            end
        end
        await 5s;
        ret = p:v;
    end
end

escape ret;
]],
    env = 'line 7 : invalid event type',
    --env = 'line 16 : wrong argument : cannot pass pointers',
    --run = { ['~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s;~>1s']=-1 },
}

Test { [[
class U with
do
end
native do
    int V = 0;
end
class Item with
    var U&& u;
do
    watching *u do
        await FOREVER;
    end
native _V;
    _V = 1;
end
do
    var U u;
    spawn Item with
        this.u = &&u;
    end;
    await 1s;
end
native _assert;
_assert(_V == 1);
escape 1;
]],
    run = { ['~>1s'] = 1 },
    --fin = 'line 18 : attribution to pointer with greater scope',
}
Test { [[
class U with
do
    await FOREVER;
end
native do
    int V = 1;
end
class Item with
    var U&& u;
do
    watching *u do
        await FOREVER;
    end
native _V;
    _V = _V+1;
end
do
    var U u;
    spawn Item with
        this.u = &&u;
    end;
    await 1s;
end
native _assert;
_assert(_V == 2);
escape 1;
]],
    run = { ['~>1s'] = 1 },
    --fin = 'line 19 : attribution to pointer with greater scope',
}
Test { [[
class U with
do
    await FOREVER;
end
native do
    int V = 1;
end
class Item with
    var U&& u;
do
    watching *u do
        await FOREVER;
    end
native _V;
    _V = _V+1;
end
do
    var U u;
    spawn Item with
        this.u = &&u;
    end;
    await 1s;
end
escape _V;
]],
    run = { ['~>1s'] = 2 },
    --fin = 'line 19 : attribution to pointer with greater scope',
}

Test { [[
class U with do end;
class Tx with
    var U&& u;
do
    watching *u do
        await FOREVER;
    end
native _V;
    _V = _V + 1;
end

native do
    int V = 0;
end

do
    var U u;
    spawn Tx with
        this.u = &&u;
    end;
    await 1s;
end
native _assert;
_assert(_V == 1);
escape _V;
]],
    run = { ['~>1s'] = 1 },
    --fin = 'line 17 : attribution to pointer with greater scope',
}
Test { [[
native do
    int V = 0;
end
class U with do end;
class Tx with
    var U&& u;
do
    watching *u do
        await FOREVER;
    end
native _V;
    _V = 1;
end
do
    var U u;
    spawn Tx with
        this.u = &&u;
    end;
    await 1s;
end
native _assert;
_assert(_V == 1);
escape 1;
]],
    run = { ['~>1s'] = 1 },
    --fin = 'line 16 : attribution to pointer with greater scope',
}
Test { [[
class U with do end;
class Tx with
    var U&& u;
do
    watching *u do
        await FOREVER;
    end
end

do
    var U u;
    spawn Tx with
        this.u = &&u;
    end;
end
escape 1;
]],
    run = 1,
    --fin = 'line 13 : attribution to pointer with greater scope',
}
Test { [[
class U with do end;
class Tx with
    var U&& u;
do
    watching *u do
        await FOREVER;
    end
end

class X with
    pool[] Tx ts;
do
    await FOREVER;
end

var X x;
do
    var U u;
    spawn Tx in x.ts with
        this.u = &&u;
    end;
end
escape 1;
]],
    run = 1,
    --fin = 'line 20 : attribution to pointer with greater scope',
}
Test { [[
class Run with
    var& int cmds;
do
end

do
    var int cmds=0;
    spawn Run with
        this.cmds = &cmds;
    end;
end

escape 1;
]],
    ref = 'line 9 : invalid attribution : variable "cmds" has narrower scope than its destination',
    --ref = 'line 9 : attribution to reference with greater scope',
}
Test { [[
class Run with
    var& int cmds;
do
end

do
    pool[] Run rs;
    var int cmds=0;
    spawn Run in rs with
        this.cmds = &cmds;
    end;
end

escape 1;
]],
    run = 1,
}

Test { [[
class Unit with
    event int move;
do
end
var Unit&&? u;
pool[] Unit units;
u = spawn Unit in units;
await 2s;
watching *u! do
    emit u!:move => 0;
end
escape 2;
]],
    run = { ['~>1min']='10] runtime error: invalid tag' },
    --fin = 'line 9 : unsafe access to pointer "u" across `await´',
}
Test { [[
class Unit with
    event int move;
do
    await FOREVER;
end
var Unit&&? u;
pool[] Unit units;
u = spawn Unit in units;
watching *u! do
    emit u!:move => 0;
end
escape 2;
]],
    run = 2,
}

Test { [[
class Unit with
    var int pos=0;
do end;

var Unit&& ptr=null;
do
    var Unit u;
    ptr = &&u;
end
ptr:pos = 0;
escape 1;
]],
    fin = 'line 8 : attribution to pointer with greater scope',
}

Test { [[
class Unit with
    var int pos;
do end;

class Tx with
    event Unit&& org;
    event int   ok;
do
    var Unit&& u = await org;
    var int pos = 1;
    watching *u do
        pos = u:pos;
    end
    await 1s;
    emit ok => pos;
end

var Tx t;
await 1s;

do
    var Unit u with
        this.pos = 10;
    end;
    emit t.org => &&u;
end

var int v = await t.ok;
escape v;
]],
    env = 'line 6 : invalid event type',
    --env = 'line 25 : wrong argument : cannot pass pointers',
    --run = { ['~>2s']=1 },
}

Test { [[
native do
    int V = 0;
end
input void OS_START,B;
class Tx with
    event void ok, go, b;
    event void e, f;
    var int v=0;
do
    v = 10;
    await e;
                            // (4)
    emit f;
                            // (6)
    v = 100;
    emit ok;
    await FOREVER;
end
var Tx a;                    // (1) v=10
var Tx&& ptr;
ptr = &&a;
watching *ptr do
    var int ret = 0;
    par/and do
        par/and do
            await OS_START;
            emit ptr:go;    // (2)
        with
            await ptr:ok;
                            // (7)
        end
        ret = ret + 1;      // ret=2
    with
        await B;
                            // (3)
        emit ptr:e;
        ret = ret + 1;
    with
        await ptr:f;
                            // (5)
        ret = ret + 1;      // ret=1
    end
native _V;
    _V = ret + ptr:v + a.v;     // _V=104
    escape ret + ptr:v + a.v;
        // this escape the outer block, which kills ptr,
        // which kills the watching, which escapes again with +1
end
escape _V + 1;
]],
    _ana = {
        --acc = 3,
    },
    run = { ['~>B']=203, }
    --run = { ['~>B']=204, }
}
Test { [[
class Unit with
    var int pos=0;
do end;

var Unit&& ptr=null;
do
    var Unit u;
    u.pos = 10;
    ptr = &&u;
end
do
    vector[100] int v;
    loop i in [0 |> 100[ do
        v[i] = i;
    end
end
escape ptr:pos;
]],
    fin = 'line 9 : attribution to pointer with greater scope',
}

Test { [[
native do
    int V = 0;
end
input void OS_START;
class Tx with
    event void ok, go;
    var int v=0, going=0;
do
    await go;
    going = 1;
    v = 10;
    emit ok;
end
var Tx a;
var Tx&& ptr;
ptr = &&a;
watching *ptr do
    par/or do
        await OS_START;
        emit a.go;
        if ptr:going then
            await FOREVER;
        end
    with
        await ptr:ok;
    end
native _V;
    _V = ptr:v + a.v;
    escape ptr:v + a.v;
end
escape _V + 1;
]],
    --run = 21,
    run = 20,
}

Test { [[
class Tx with
    var int v = 0;
do
    await FOREVER;
end
pool[1] Tx ts;
var Tx&&? ok1 = spawn Tx in ts with
                this.v = 10;
              end;
watching *ok1! do
    var int ok2 = 0;// spawn Tx in ts;
    var int ret = 0;
    loop t in ts do
        ret = ret + t:v;
    end
    escape (ok1?) + ok2 + ret;
end
escape 1;
]],
    run = 11,
    --run = 1,
}

Test { [[
native do
    int V = 0;
end
class Tx with
    var int v = 0;
do
    async do end;
end
pool[1] Tx ts;
var Tx&&? ok1 = spawn Tx in ts with
                this.v = 10;
              end;
watching *ok1! do
    var int ok2 = 0;// spawn Tx in ts;
    var int ret = 0;
    loop t in ts do
        ret = ret + t:v;
    end
native _V;
    _V = (ok1?) + ok2 + ret;
    escape (ok1?) + ok2 + ret;
end
escape _V + 1;  // this one executes because of strong abortion in the watching
]],
    _ana = {
        acc = true,
    },
    run = 11,
    --run = 12,
}

Test { [[
class Tx with
    event (int,int) ok_game;
do
    await 1s;
    emit this.ok_game => (1,2);
end
var Tx t;
var Tx&& i = &&t;
var int a,b;
watching *i do
    (a,b) = await i:ok_game;
    emit i:ok_game => (a,b);
end
escape a+b;
]],
    run = { ['~>1s']=3 },
}


Test { [[
input void OS_START;

class Tx with
do
    event void x;
    par/or do
        await x;
    with
        await OS_START;
        emit x;
    end
end

do
    var Tx t;
    await OS_START;
end

escape 10;
]],
    run = 10,
}
Test { [[
input void OS_START;

class U with
    event void x;
do
    await x;
end

class Tx with
    var U&& u;
do
    watching *u do
        await OS_START;
        emit u:x;
    end
end

do
    var U u;
    var Tx t with
        this.u = &&u;
    end;
    await OS_START;
end

escape 10;
]],
    wrn = true,
    run = 10,
}

Test { [[
class V with
do
end

input void OS_START;
class U with
    var V&&? v;
    event void x;
do
    loop do
        await x;
        v = spawn V;
        break;
    end
end

class Tx with
    var U&& u;
do
    watching *u do
        await OS_START;
        emit u:x;
native _assert;
        _assert(0);
    end
end

do
    var U u;
    var Tx t with
        this.u = &&u;
    end;
    await OS_START;
end

escape 10;
]],
    wrn = true,
    run = 10,
    --fin = 'line 12 : pointer access across `await´',
    --fin = 'line 12 : invalid block for awoken pointer "v"',
}
Test { [[
interface UI with
end

class Tx with
    interface UI;
do
end

class UIGridItem with
    var UI&& ui;
do
    watching *ui do
        await FOREVER;
    end
end

class UIGridPool with
    pool[] UIGridItem all;
do
    await FOREVER;
end

class UIGrid with
    var& UIGridPool uis;
do
end

do
    var UIGridPool pool1;
    var UIGrid g1 with
        this.uis = &pool1;
    end;

    var Tx g2;
    spawn UIGridItem in g1.uis.all with
        this.ui = &&g2;
    end;
end

escape 1;
]],
    --fin = 'line 36 : attribution requires `finalize´',
    run = 1,
}
Test { [[
interface UI with
end

class Tx with
    interface UI;
do
end

class UIGridItem with
    var UI&& ui;
do
    watching *ui do
        await FOREVER;
    end
end

class UIGridPool with
    pool[] UIGridItem all;
do
    await FOREVER;
end

class UIGrid with
    var& UIGridPool uis;
do
end

    var UIGridPool pool1;
    var UIGrid g1 with
        this.uis = &pool1;
    end;

    var Tx g2;
    spawn UIGridItem in g1.uis.all with
        this.ui = &&g2;
    end;
escape 1;
]],
    --fin = 'line 35 : attribution requires `finalize´',
    run = 1,
}

Test { [[
interface Screen with
    var& _GUIScreen? me;
end

interface IWorldmapScreen with
    interface Screen;
end

class WorldmapScreen with
    interface IWorldmapScreen;
do
end

var WorldmapScreen&&? ws = spawn WorldmapScreen with
    this.me = &_new_GUIScreen();
end;

escape 1;
]],
    fin = 'line 15 : attribution requires `finalize´',
}
Test { [[
interface UI with
end

class Tx with
    interface UI;
do
end

class UIGridItem with
    var UI&& ui;
do
    watching *ui do
        await FOREVER;
    end
end

class UIGridPool with
    pool[] UIGridItem all;
do
    await FOREVER;
end

class UIGrid with
    var& UIGridPool uis;
do
end

do
    var UIGridPool pool1;
    var UIGrid g1 with
        this.uis = &pool1;
    end;

    var Tx g2;
    spawn UIGridItem in pool1.all with
        this.ui = &&g2;
    end;
end

escape 1;
]],
    run = 1,
}

Test { [[
native do
    int V = 0;
end
input void OS_START;

interface I with
    var int e;
end

class Tx with
    var int e=0;
do
    e = 100;
    await FOREVER;
end

var Tx t;
var I&& i = &&t;
watching *i do
    await OS_START;
native _V;
    _V = i:e;
    escape i:e;
end
escape _V + 1;
]],
    run = 100,
    --run = 101,
}

Test { [[
native do
    int V = 0;
end

input void OS_START;

interface I with
    event void e;
    var int ee;
end

class Tx with
    event void e;
    var int ee=0;
do
    await e;
    ee = 100;
    await FOREVER;
end

var Tx t;
var I&& i = &&t;

watching *i do
    await OS_START;
    emit i:e;
native _V;
    _V = i:ee;
    escape i:ee;
end
escape _V + 1;
]],
    run = 100,
    --run = 101,
}

Test { [[
native do
    int V = 0;
end

input void OS_START;

interface I with
    event int e, f;
    var int vv;
end

class Tx with
    event int e, f;
    var int vv=0;
do
    var int v = await e;
    vv = v;
    emit f => v;
    await FOREVER;
end

var Tx t1;
var I&& i1 = &&t1;

watching *i1 do
    var int ret = 0;
    par/and do
        await OS_START;
        emit i1:e => 99;            // 21
    with
        var int v = await i1:f;
        ret = ret + v;
    with
        await OS_START;
    end
native _V;
    _V = ret;
    escape ret;
end
escape _V+1;
]],
    --run = 100,
    run = 99,
}

Test { [[
native do
    int V = 0;
end

input void OS_START;

interface I with
    event int e, f;
end

class Tx with
    event int e, f;
do
    var int v = await e;
    emit f => v;
    await FOREVER;
end

var Tx t1, t2;
var I&& i1 = &&t1;

watching *i1 do
    var I&& i2 = &&t2;
    watching *i2 do
        var int ret = 0;
        par/and do
            await OS_START;
            emit i1:e => 99;            // 21
        with
            var int v = await i1:f;
            ret = ret + v;
        with
            await OS_START;
            emit i2:e => 66;            // 27
        with
            var int v = await i2:f;
            ret = ret + v;
        end
native _V;
        _V = ret;
        escape ret;
    end
end
escape _V + 1;
]],
    _ana = {
        acc = true,
    },
    run = 165,
    --run = 166,
}

Test { [[
native do
    int V = 0;
end

interface I with
    var int v;
    code/instantaneous Fx (var int)=>void;
end

class Tx with
    var int v=0;
    code/instantaneous Fx (var int)=>void;
do
    v = 50;
    this.Fx(10);

    code/instantaneous Fx (var int v)=>void do
        this.v = this.v + v;
    end
    await FOREVER;
end

var Tx t;
var I&& i = &&t;
input void OS_START;
watching *i do
    await OS_START;
    i:Fx(100);
native _V;
    _V = i:v;
    escape i:v;
end
escape _V+1;
]],
    wrn = true,
    run = 160,
    --run = 161,
}

Test { [[
native do
    int V = 0;
end

interface I with
    var int v;
    code/instantaneous Fx (var int)=>void;
end

class Tx with
    interface I;
    var int v=0;
do
    v = 50;
    this.Fx(10);

    code/instantaneous Fx (var int a)=>void do
        v = v + a;
    end
    await FOREVER;
end

var Tx t;
var I&& i = &&t;
input void OS_START;
watching *i do
    await OS_START;
    i:Fx(100);
native _V;
    _V = i:v;
    escape i:v;
end
escape _V+1;
]],
    run = 160,
    --run = 161,
}

Test { [[
interface I with
    var int v;
    code/instantaneous Get (void)=>int;
    code/instantaneous Set (var int)=>void;
end

class Tx with
    interface I;
    var int v = 50;
do
    code/instantaneous Get (void)=>int do
        escape v;
    end
    code/instantaneous Set (var int v)=>void do
        this.v= v;
    end
    await FOREVER;
end

var Tx t;
var I&& i = &&t;
var int v = i:v;
i:set(100);
escape v + i:get();
]],
    wrn = true,
    run = 150,
}

Test { [[
native do
    int V = 0;
end

interface I with
    var int v;
    code/instantaneous Fx (var int)=>void;
end

class Tx with
    interface I;
    var int v=0;
do
    v = 50;
    this.Fx(10);

    code/instantaneous Fx (var int v)=>void do
        this.v = this.v + v;
    end
    await FOREVER;
end

class U with
    interface I;
    var int v=0;
do
    v = 50;
    this.Fx(10);

    code/instantaneous Fx (var int v)=>void do
        this.v = this.v + 2*v;
    end
    await FOREVER;
end

var Tx t;
var U u;
var I&& i = &&t;
input void OS_START;
watching *i do
    await OS_START;
    i:Fx(100);
    var int ret = i:v;

    i=&&u;
    i:Fx(200);
native _V;
    _V = ret + i:v;
    escape ret + i:v;
end
escape _V+1;
]],
    wrn = true,
    run = 630,
    --run = 631,
}

Test { [[
class Tx with
    var int v = 0;
do
    this.v = 10;
end

var int ret = 1;
var Tx&&? t = spawn Tx;
if t? then
    watching *t! do
        do finalize with
            ret = t!:v;
        end
        await FOREVER;
    end
end

escape ret;
]],
    run = 1,
}

Test { [[
class Tx with
    var int v = 0;
do
    this.v = 10;
end

var Tx&&? t = spawn Tx;
watching *t! do
    await FOREVER;
end

escape t!:v;
]],
    run = '8] runtime error: invalid tag',
    --fin = 'line 12 : unsafe access to pointer "t" across `await´',
}

Test { [[
class Tx with
    var int v = 0;
do
    this.v = 10;
end

var Tx&&? t = spawn Tx;
watching *t! do
    await FOREVER;
end

await 1s;

escape t!:v;
]],
    run = '8] runtime error: invalid tag',
    --fin = 'line 14 : unsafe access to pointer "t" across `await´',
}

Test { [[
input void OS_START;
class Tx with
    var int id = 0;
do
    await OS_START;
end

pool[9999] Tx ts;
var Tx&& t0 = null;
loop i in [0 |> 9999[ do
    var Tx&&? t = spawn Tx with
        this.id = 9999-i;
    end;
    if t0 == null then
        t0 = t!;
    end
end

watching *t0 do
    await FOREVER;
end
var int ret = t0:id;

escape ret;
]],
    fin = 'line 14 : unsafe access to pointer "t0" across `loop´ (tests.lua : 10)',
    --run = 9999,
}

Test { [[
input void OS_START;
class Tx with
    var int id = 0;
do
    await OS_START;
end

pool[9999] Tx ts;
loop i in [0 |> 9999[ do
    var Tx&& t0 = null;
    var Tx&&? t = spawn Tx with
        this.id = 9999-i;
    end;
    if t0 == null then
        t0 = t!;
    end
end

escape 1;
]],
    fin = 'line 14 : unsafe access to pointer "t0" across `spawn´',
    --run = 9999,
}

Test { [[
class Tx with
    var int v = 10;
do
    await 1s;
end

var Tx t1;
var Tx&&? ptr = &&t1;
await 1s;
var Tx t2;
ptr = &&t2;
await 200ms;

escape ptr!:v;
]],
    run = { ['~>10s']=10 },
}

-- UNTIL

Test { [[
input int A;
var int x = await A until x>10;
escape x;
]],
    run = {
        ['1~>A; 0~>A; 10~>A; 11~>A'] = 11,
    },
}

Test { [[
native do
    int V = 0;
end
input int A;
var int v = 0;
par/or do
    every 10s do
native _V;
        _V = _V + 1;
    end
with
    await 10s until v;
with
    await 10s;
    v = 1;
    await FOREVER;
end
escape _V;
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['~>1min'] = 2,
    },
}

Test { [[
native do
    int V = 0;
end
input int A;
var int v = 0;
par/or do
    every 10s do
native _V;
        _V = _V + 1;
    end
with
    await 10s until v;
with
    await 20s;
    v = 1;
    await FOREVER;
end
escape _V;
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['~>1min'] = 3,
    },
}

Test { [[
input int A;
var int v = 0;
par do
    await 10s until v;
    escape 10;
with
    await 10min;
    v = 1;
end
]],
    _ana = {
        acc = 1,
    },
    run = {
        ['~>10min10s'] = 10,
    },
}

Test { [[
input void OS_START;

interface Global with
    var int x;
end

var int x = 10;

class Tx with
    var int x=0;
do
    this.x = global:x;
end

var Tx t;
await OS_START;
escape t.x;
]],
    run = 10,
}

Test { [[
input int A, E;
var int n_shields = 0;
var int ret = 1;
par/or do
    await A;
with
    loop do
        var int v = await E until (n_shields > 0);
        ret = ret + v;
    end
end

escape ret;
]],
    run = { ['1~>E; 1~>E; 1~>A'] = 1 }
}
Test { [[
input void A, E;
var int n_shields = 0;
var int ret = 1;
par/or do
    await A;
with
    loop do
        await E until (n_shields > 0);
        ret = ret + 10;
    end
end

escape ret;
]],
    run = { ['~>E; ~>E; ~>A'] = 1 }
}

Test { [[
interface Controller with
    var float ax;
end
class KeyController with
    interface Controller;
    var int ax = 0;
do
end

var KeyController c;
var Controller&&   i;
i = &&c;
escape 1;
]],
    wrn = true,
    env = 'line 12 : types mismatch (`Controller&&´ <= `KeyController&&´)',
}

Test { [[
interface Controller with
    var float ax;
end
class KeyController with
    interface Controller;
    var float ax = 0;
do
end

var KeyController c;
var Controller&&   i;
i = &&c;
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
input (int,int) I;
var int ret = 0;
par/or do
    loop do
        var int a,b;
        (a,b) = await I
                until a == 1;
        ret = ret + a + b;
    end
with
    await 2s;
    await 2s;
with
    async do
        emit I => (1,2);
        emit I => (1,2);
        emit 5s;
    end
end
escape ret;
]],
    run = 6,
}

-- AWAITS // Await MANY // SELECT

--[=[
Test { [[
await (10ms);
escape 1;
]],
    parser = 'line 1 : after `)´ : expected `or´',
}
Test { [[
await (10ms) or (20ms);
escape 1;
]],
    env = 'line 1 : invalid await: multiple timers',
}
Test { [[
await ((10)ms);
escape 1;
]],
    parser = 'line 1 : after `)´ : expected `or´',
}

Test { [[
await (e) or
      (f);
escape 1;
]],
    locs = 'line 1 : internal identifier "e" is not declared',
}

Test { [[
event void e;
var int f;
await (e) or
      (f);
escape 1;
]],
    env = 'line 3 : event "f" is not declared',
}

Test { [[
event void e;
event int f;
input void OS_START;
await (e) or
      (f) or
      (OS_START);
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
await (10ms) or (OS_START);
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
var int&& x = await (10ms) or (OS_START);
escape 1;
]],
    env = 'line 2 : invalid attribution',
}

Test { [[
input void OS_START;
par/or do
    loop do
        await (OS_START) or (OS_START);
    end
with
    await OS_START;
end
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
await (10ms) or (OS_START)
        until 1;
escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
var int i = await (10ms) or (OS_START)
        until i==1;
escape i;
]],
    run = 1,
}
Test { [[
input void OS_START;
var int i = await (10ms) or (OS_START)
        until i==0;
escape i+1;
]],
    run = {['~>10ms']=1},
}
]=]

--do return end

-- GLOBAL AWAITS (deprecated)

Test { [[
input void A, B;
loop do
    if true then
        await B;
    end
    await A;
end
]],
    _ana = {
        isForever = true,
    },
    awaits = 0,
    run = false,
}

Test { [[
input void A, B;
loop do
    if true then
        await B;
    else
        await A;
        await A;
    end
end
]],
    _ana = {
        isForever = true,
    },
    awaits = 0,
    run = false,
}

Test { [[
input void A, B;
loop do
    if true then
        await B;
    end
end
]],
    _ana = {
        isForever = true,
    },
    awaits = 0,
    loop = true,
    run = false,
}

Test { [[
input void A;
await A;
loop i in [0 |> 10[ do
end
escape 1;
]],
    awaits = 0,
    --loop = true,
    run = { ['~>A'] = 1 },
}

Test { [[
input void A;
loop do
    await A;
end
await FOREVER;
]],
    _ana = {
        isForever = true,
    },
    awaits = 0,     -- stmts
}

Test { [[
input void A;
loop do
    await A;
end
escape 1;
]],
    _ana = {
        isForever = true,
    },
    awaits = 0,     -- stmts
    run = false,
}

Test { [[
input void A,B;
    par do
        loop do
            await A;
        end
    with
        loop do
            await B;
        end
    end
]],
    _ana = {
        isForever = true,
    },
    awaits = 2,
    run = false,
}

Test { [[
input void A,B, X;
loop do
    par/or do
        loop do
            await A;
        end
        await X;
    with
        await X;
        loop do
            await B;
        end
    end
end
]],
    _ana = {
        isForever = true,
    },
    awaits = 0,
    run = false,
}

Test { [[
input void A,B, X;
loop do
    par/or do
        loop do
            await A;
        end
    with
        await X;
        loop do
            await A;
        end
    end
end
]],
    _ana = {
        isForever = true,
    },
    awaits = 1,
    run = false,
}

Test { [[
input void A, B;
class Tx with
    event void e;
do
    await A;
    await A;
end
var Tx a,b;
native _f;
var int c=0;
par do
    loop do
        _f();
        await A;
        if false then
            break;
        end
    end
with
    loop do
        await 2s;
        if _f() then
            break;
        end
    end
with
    loop do
        await B;
        c = 1;
    end
with
    loop do
        await a.e;
    end
with
    loop do
        await b.e;
    end
end
]],
    _ana = {
        isForever = true,
    },
    gcc = 'error: implicit declaration of function',
    awaits = 3,
}
Test { [[
input void A, B;
class Tx with
    event void e;
do
    await A;
    await A;
end
var Tx a,b;
native _f;
var int c=0;
par do
    loop do
        _f();
        await A;
        if false then
            break;
        end
    end
with
    loop do
        await 2s;
        if _f() then
            break;
        end
    end
with
    loop do
        await B;
        c = 1;
    end
with
    loop do
        await a.e;
    end
with
    loop do
        await b.e;
    end
end
]],
    safety = 2,
    _ana = {
        acc = 1,
        isForever = true,
    },
    awaits = 3,
    gcc = 'error: implicit declaration of function',
}
--do return end

Test { [[
input int A, B;
class Tx with
    event int e;
do
    await A;
    await A;
end
var Tx a,b;
native _f;
var int c=0;
par do
    loop do
        _f();
        var int x = await A;
        if false then
            break;
        end
    end
with
    loop do
        await 2s;
        if _f() then
            break;
        end
    end
with
    loop do
        var int x = await B;
        c = 1;
    end
with
    loop do
        var int x = await a.e;
    end
with
    loop do
        await b.e;
    end
end
]],
    _ana = {
        isForever = true,
    },
    awaits = 1,
    gcc = 'error: implicit declaration of function',
}

Test { [[
input void A, B, X;
loop do
    par/or do
        await A;
    with
        await B;
    end
    await X;
end
]],
    _ana = {
        isForever = true,
    },
    awaits = 0,
    run = false,
}

Test { [[
input void A;
class Tx with
do
    loop do
        await A;
    end
end
par do
    loop do
        await A;
    end
with
    await FOREVER;
end
]],
    _ana = {
        isForever = true,
    },
    run = false,
    awaits = 1,
}

Test { [[
input void A;
class Tx with
do
    loop do
        await A;
    end
end
par do
    loop do
        await A;
    end
with
    var Tx a;
    await FOREVER;
end
]],
    _ana = {
        isForever = true,
    },
    run = false,
    awaits = 2,
}

-- TUPLES

Test { [[
var int a, b;
(a) = 1;
escape 1;
]],
    --parser = 'line 2 : before `=´ : expected `,´',
    --env = 'line 2 : arity mismatch',
    run = 1,
}

Test { [[
var int a, b;
(a,b) = 1;
escape 1;
]],
    parser = 'line 2 : after `=´ : expected `request´ or `await´ or `watching´ or `(´',
    --parser = 'line 2 : before `=´ : expected `,´',
    --env = 'line 2 : arity mismatch',
    --run = 1,
}

Test { [[
input (int) A;
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
native _int;
input (_int,int) A;
escape 1;
]],
    wrn = true,
    run = 1;
}

Test { [[
input (int&&,int) A;
//event (int,int&&) a;
escape 1;
]],
    wrn = true,
    run = 1;
}

Test { [[
input (int,int) A;
event (int,int) a;
escape 1;
]],
    wrn = true,
    run = 1;
}

Test { [[
input (int,int) LINE;
var int v;
v = await LINE;
escape 1;
]],
    todo = 'arity error',
}

Test { [[
input (int,int) A;
par/or do
    event int a,b;
    (a,b) = await A;
    escape 1;
with
    async do
        emit A => (1,2);
    end
end
escape 1;
]],
    env = 'line 4 : wrong argument #1',
    --env = 'line 4 : invalid attribution',
}

Test { [[
input (int,int&&) A;
par/or do
    var int a,b;
    (a,b) = await A;
    escape a + b;
with
    async do
        emit A => (1,2);
    end
end
escape 1;
]],
    env = 'line 4 : wrong argument #2',
}

Test { [[
input (int,int&&) A;
par/or do
    var int a,b;
    (a,b) = await A;
    escape a + b;
with
    async do
        var int x = 2;
        emit A=> (1,&&x);
    end
end
escape 1;
]],
    env = 'line 4 : wrong argument #2',
}

Test { [[
input (int,int) A;
par/or do
    var int a,b;
    (a,b) = await A;
    escape a + b;
with
    async do
        emit A => (1,2);
    end
end
escape 1;
]],
    run = 3;
}

Test { [[
event (int,int) a;
par/or do
    var int a,b;
    (a,b) = await a;
    escape a + b;
with
    async (a) do
        emit a => (1,2);
    end
end
escape 1;
]],
    wrn = true,
    env = 'line 4 : event "a" is not declared',
}

Test { [[
event (int,int) a;
input void OS_START;
par/or do
    var int c,d;
    (c,d) = await a;
    escape c + d;
with
    await OS_START;
    emit a => (1,2);
end
escape 1;
]],
    run = 3,
}

Test { [[
event (int,int) e;
emit e => (1,2,3);
escape 1;
]],
    env = 'arity mismatch',
    --env = 'line 2 : invalid attribution (void vs int)',
}

-- INCLUDE

Test { [[
native do
    ##include <stdio.h>
    ##include <stdio.h>
end
escape 1;
]],
    run = 1,
}

Test { [[
#include
escape 1;
]],
    lines = 'error: #include expects "FILENAME" or <FILENAME>',
}

Test { [[
#include "MOD1"
#include "http://ceu-lang.org/"
#include "https://github.com/fsantanna/ceu"
#include "^4!_"
escape 1;
]],
    lines = 'fatal error: MOD1: No such file or directory',
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
input void A;
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
await A;
escape 1;
]],
    run = { ['~>A']=1 },
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
nothing;
nothing;
nothing;
input void A
]])
Test { [[
nothing;
#include "/tmp/_ceu_MOD1.ceu"
await A;
escape 1;
]],
    parser = '/tmp/_ceu_MOD1.ceu : line 4 : after `A´ : expected `,´ or `;´',
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
input void A;
native do ##include <assert.h> end
native _assert;
_assert(0);
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
await A;
escape 1;
]],
    --run = { ['~>A']=1 },
    run = "ceu_app_go: Assertion `0' failed",
}

INCLUDE('/tmp/_ceu_MOD2.ceu', [[
input void A;
]])
INCLUDE('/tmp/_ceu_MOD1.ceu', [[
#include "/tmp/_ceu_MOD2.ceu"
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
await A;
escape 1;
]],
    run = { ['~>A']=1 },
}

INCLUDE('/tmp/_ceu_MOD2.ceu', [[
input void A;
nothing
]])
INCLUDE('/tmp/_ceu_MOD1.ceu', [[
#include "/tmp/_ceu_MOD2.ceu"
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
await A;
escape 1;
]],
    parser = '/tmp/_ceu_MOD2.ceu : line 2 : after `nothing´ : expected `;´',
}

INCLUDE('/tmp/_ceu_MOD2.ceu', [[
input void A;
]])
INCLUDE('/tmp/_ceu_MOD1.ceu', [[
input void A;
]])
INCLUDE('/tmp/_ceu_MOD0.ceu', [[
#include "/tmp/_ceu_MOD1.ceu"
#include "/tmp/_ceu_MOD2.ceu"
]])
Test { [[
#include "/tmp/_ceu_MOD0.ceu"
await A;
escape 1;
]],
    tops = '/tmp/_ceu_MOD2.ceu : line 1 : identifier "A" is already declared (/tmp/_ceu_MOD1.ceu : line 1)',
    wrn = true,
    run = { ['~>A']=1 },
}

INCLUDE('/tmp/_ceu_MOD2.ceu', [[
input void A;
]])
INCLUDE('/tmp/_ceu_MOD1.ceu', [[
nothing;
input void A
]])
INCLUDE('/tmp/_ceu_MOD0.ceu', [[
#include "/tmp/_ceu_MOD2.ceu"
#include "/tmp/_ceu_MOD1.ceu"
]])
Test { [[
#include "/tmp/_ceu_MOD0.ceu"
await A;
escape 1;
]],
    parser = '/tmp/_ceu_MOD1.ceu : line 2 : after `A´ : expected `,´ or `;´',
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
native do
    int f () {
        escape 10;
    }
end
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
native _f;
escape _f();
]],
    run = 10,
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
native do
    int f () {
        escape 10;
    }
end
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
#include "/tmp/_ceu_MOD1.ceu"
native _f;
escape _f();
]],
    gcc = 'error: redefinition of',
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
#ifndef MOD1
#define MOD1
native do
    int f () {
        escape 10;
    }
end
#endif
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
#include "/tmp/_ceu_MOD1.ceu"
native _f;
escape _f();
]],
    run = 10,
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
interface Tx with
    var int i;
end
var int i = 0;
]])
Test { [[
//
//
#include "/tmp/_ceu_MOD1.ceu"
interface Tx with
    var int i;
end
var int i = 10;
escape i;
]],
    env = 'line 4 : top-level identifier "Tx" already taken',
    --env = 'tests.lua : line 4 : interface/class "Tx" is already declared',
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
interface Tx with
    var int i;
end
var int i = 0;
]])
Test { [[
//
//
interface Tx with
    var int i;
end
#include "/tmp/_ceu_MOD1.ceu"
var int i = 10;
escape i;
]],
    env = 'line 1 : top-level identifier "Tx" already taken',
    --env = '/tmp/_ceu_MOD1.ceu : line 1 : interface/class "Tx" is already declared',
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
interface Global with
    var int i;
end
var int i = 0;
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
interface Global with
    var int i;
end
var int i = 10;
escape i;
]],
    env = 'line 2 : top-level identifier "Global" already taken',
    --env = 'line 2 : interface/class "Global" is already declared',
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
#ifndef GLB
#define GLB
interface Global with
    var int i;
end
#endif
var int i = 0;
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
#ifndef GLB
interface Global with
    var int i;
end
#endif
var int i = 10;
escape i;
]],
    wrn = true,
    run = 10,
}

INCLUDE('/tmp/_ceu_MOD1.ceu', [[
native do
    int f () {
        escape 10;
    }
    int A;
    int B;
end
]])
Test { [[
#include "/tmp/_ceu_MOD1.ceu"
native _f;
escape _f();
]],
    run = 10,
}

Test { [[
native do
    ##include <unistd.h>
end
escape 1;
]],
    run = 1,
}

-- CLASSES/THREADS

Test { [[
class Tx with
    event int ok;
do
    var int v=0;
    var& int p = &v;
    async/thread (p) do
        var int ret = 0;
        loop i in [0 |> 50000[ do
            loop j in [0 |> 50000[ do
                ret = ret + i + j;
            end
        end
        atomic do
            p = ret;
        end
    end
    emit ok => v;
end

var Tx t1, t2;
var int v1=0, v2=0;

par/and do
    v1 = await t1.ok;
with
    v2 = await t2.ok;
end

native do ##include <assert.h> end
native _assert;
_assert(v1 == v2);
escape v1;
]],
    run = 1066784512,
    --run = false,
-- thr.c
--./a.out  17.41s user 0.00s system 180% cpu 9.629 total
-- me (isTmp=true)
--./a.out  16.80s user 0.02s system 176% cpu 9.525 total
-- me (isTmp=false)
--./a.out  30.36s user 0.04s system 173% cpu 17.476 total
}

-- CLASSES/REFS / &

Test { [[
class Tx with
    var int x=0;
do
end
class U with do end;
event Tx& e;
par/and do
   do
      await 1s;
      var Tx t;
      emit e => t;
   end
   var U u;
with
   var& Tx t = await e;
   t.x = 1;
   await 1s;
end
escape 1;
]],
    parser = 'line 6 : after `Tx´ : expected type modifier or internal identifier',
    --env = 'line 6 : invalid event type',
}

Test { [[
class Tx with
    var int x=0;
do
end
class U with do end;
event (Tx&,int) e;
par/and do
   do
      await 1s;
      var Tx t;
      emit e => (t,1);
   end
   var U u;
with
   var& Tx t;
   var int i;
   (t,i) = await e;
   t.x = 1;
   await 1s;
end
escape 1;
]],
    parser = 'line 6 : after `Tx´ : expected type modifier or `,´ or `)´',
    --parser = 'line 6 : after `Tx´ : expected `,´',
    --env = 'line 6 : invalid event type',
    --run = 1,
}

Test { [[
var& int i = 1;
escape 1;
]],
    ref = 'line 1 : invalid attribution',
}

Test { [[
var int&& p=null;
var& int i = *p;
escape 1;
]],
    ref = 'line 2 : invalid attribution',
}

Test { [[
event int e;
var& int i = await e;
escape 1;
]],
    ref = 'line 2 : invalid attribution',
}

Test { [[
event int& e;
var& int i = await e;
escape 1;
]],
    parser = 'line 1 : after `int´ : expected type modifier or internal identifier',
}

Test { [[
native/plain _t;
native/nohold _f;
pre native do
    #define f(a)
    typedef int t;
end
class Tx with
    var& _t t;
do
    await 1s;
    _f(&&t);
end
escape 1;
]],
    run = 1,
}

Test { [[
interface I with end;
class Tx with
    var I&& i = null;
do
end

var Tx t;
await 1s;
native _assert;
_assert(t.i == null);
escape 1;
]],
    run = { ['~>1s'] = 1 },
}

Test { [[
interface I with end;
class Tx with
    var I&& i = null;
do
end

var Tx t;
var I&& i = t.i;
await 1s;
native _assert;
_assert(t.i == null);
escape 1;
]],
    fin = 'line 10 : unsafe access to pointer "i" across `await´',
    --run = { ['~>1s'] = 1 },
}

Test { [[
interface I with end;
class Tx with
    var I&& i = null;
do
end

var Tx t;
var I&& i = t.i;
await 1s;
native _assert;
_assert(i == null);
escape 1;
]],
    fin = 'line 10 : unsafe access to pointer "i" across `await´',
}

Test { [[
class Tx with do end

class Pool with
    pool[] Tx all;
do
    await FOREVER;
end

interface Global with
    var Pool&& p;
end
var Pool&& p = null;

class S with
do
    await 1s;
    spawn Tx in global:p:all with
    end;
end

escape 1;
]],
    fin = 'line 17 : unsafe access to pointer "p" across `class´',
}

Test { [[
native/plain _t;
pre native do
    typedef struct t {
        int v;
    } t;
end

class Unit with
    var _t t;
do
end

var Unit u with
    this.t = _t(30);
end;
escape u.t.v;
]],
    run = 30,
}

Test { [[
class Map with
    event (int,int) go_xy;
do
end

var Map m1;
var Map&& m=&&m1;
emit m:go_xy => (1,1);

escape 1;
]],
    run = 1,
}

Test { [[
input void OS_START;
var int a = 1;
event int& e;
par do
    var& int v = await e;
    v = v + 1;
with
    await OS_START;
    var int b = 10;
    emit e => b;
    escape b;
end
]],
    parser = 'line 3 : after `int´ : expected type modifier or internal identifier',
    --env = 'line 3 : invalid event type',
    --run = 11,
}

Test { [[
input void OS_START;
var int a = 1;
event (int,int&) e;
par do
    var& int r;
    var int  v;
    (v,r) = await e;
    r = r + v;
with
    await OS_START;
    var int b = 10;
    emit e => (4,b);
    escape b;
end
]],
    parser = 'line 3 : after `int´ : expected type modifier or `,´ or `)´',
    --run = 14,
}

Test { [[
interface Object with
    var _SDL_Rect rect;
end
class MoveObject with
    var Object&& obj = null;
do
native _assert;
    _assert(this.obj != null);
    await 1s;
    obj:rect.x = 1;
end
escape 1;
]],
    fin = 'line 9 : unsafe access to pointer "obj" across `await´',
}

Test { [[
native/plain _int;
interface Object with
    var _int v;
end
class MoveObject with
    var& Object obj;
do
    await 1s;
    obj.v = 1;
end
escape 1;
]],
    run = 1,
}
Test { [[
native/plain _int;
interface Object with
    var _int v;
end
class MoveObject with
    var& Object obj;
do
    await 1s;
    obj.v = 1;
end
class X with
    interface Object;
    var _int v=0;
do
end
var X xxx;
var MoveObject m with
    this.obj = &xxx;
end;
escape 1;
]],
    run = 1,
}
Test { [[
native/plain _int;
interface Object with
    var _int v;
end
class O with
    var _int v=0;
    interface Object;
do
    this.v = 10;
end
class MoveObject with
    var& Object obj;
do
    await 1s;
    obj.v = 1;
end
var O o;
escape o.v;
]],
    run = 10,
}
Test { [[
class Tx with
    var int v = 0;
do
end
var Tx t with
    this.v = 10;
end;
var& Tx tt = &t;
tt.v = 5;
escape t.v;
]],
    run = 5,
}

Test { [[
native/plain _int;
interface Object with
    var _int v;
end
class O with
    var _int v=0;
    interface Object;
do
    this.v = 10;
end
class MoveObject with
    var _int v=0;
    var& Object obj;
do
    await 1s;
    obj.v = 1;
end
var O o;
var MoveObject m with
    this.obj = &o;
end;
await 2s;
escape o.v;
]],
    run = { ['~>2s']=1 },
}

Test { [[
class Parser with
    event int  evtByte;
    event void evtStop;
do end;

class Frame with
    code/instantaneous RawWriteByte (var int)=>void;
do
    code/instantaneous RawWriteByte (var int v)=>void do if v then end end;
end;

class Receiver with
    var& Parser up;
    var& Frame rx;
    event void evtReady;
do
    par do
        every pB in up.evtByte do
            rx.rawWriteByte(pB);
        end
    with
        every up.evtStop do
            emit evtReady;
        end
    end
end

escape 1;
]],
    run = 1,
}

Test { [[
class U with do end;

class Tx with
    var U&& u;
do
    watching *u do
    end
end

var U&& u = null;
var Tx t with
    this.u = u;
end;

escape 1;
]],
    valgrind = false,
    run = 'SEGFAULT',
}

--<<< CLASSES, ORGS, ORGANISMS

-->>> INTERFACE / BLOCKI / INPUT / OUTPUT / INPUT/OUTPUT / OUTPUT/INPUT

Test { [[
class Tx with
    input:
        var int i;
do
end
var Tx t;
escape t.i;
]],
    ref = 'line 6 : missing initialization for field "i" (declared in tests.lua:3)',
    --mode = 'line 7 : cannot read field with mode `input´',
}

Test { [[
class Tx with
    output:
        var int o;
do
end
var Tx t with
    this.o  = 1;
end;
escape 1;
]],
    mode = 'line 7 : cannot write to field with mode `output´',
}

Test { [[
class Tx with
    output:
        var int o;
do
end
var Tx t;
t.o = 1;
escape 1;
]],
    mode = 'line 7 : cannot write to field with mode `output´',
}

Test { [[
class Tx with
    input:
        var int i;
do
    i  = 1;
end
escape 1;
]],
    mode = 'line 5 : cannot write to field with mode `input´',
}
Test { [[
class Tx with
    input:
        var int i;
do
    this.i  = 1;
end
escape 1;
]],
    mode = 'line 5 : cannot write to field with mode `input´',
}

Test { [[
class Tx with
    input:
        var int i;

    output:
        var int o;

    input/output:
        var int io;

    output/input:
        var int oi;
do
    o  = 1;
    io = 1;
    oi = 1;
end
var Tx t with
    this.i  = 1;
    this.io = 1;
    this.oi = 1;
end;
t.i  = 1;
t.io = 1;
t.oi = 1;
escape t.o+t.io+t.oi;
]],
    run = 3,
}
Test { [[
class Tx with
    input:
        var int i;

    output:
        var int o;

    input/output:
        var int io;

    output/input:
        var int oi;
do
    this.o  = 1;
    this.io = 1;
    this.oi = 1;
end
var Tx t with
    this.i  = 1;
    this.io = 1;
    this.oi = 1;
end;
t.i  = 1;
t.io = 1;
t.oi = 1;
escape t.o+t.io+t.oi;
]],
    run = 3,
}

Test { [[
class Tx with
    input/output:
        var& int io;
do
    var int io_ = 1;
    io = &io_;
end
escape 1;
]],
    ref = 'line 6 : invalid attribution : variable "io" is already bound',
}
Test { [[
class Tx with
    input/output:
        var& int io;
do
    var int io_ = 1;
    this.io = &io_;
end
escape 1;
]],
    ref = 'line 6 : invalid attribution : variable "io" is already bound',
}

Test { [[
class Tx with
    output/input:
        var& int oi;
do
    var int oi_=0;
    oi = &oi_;
end

var int oi = 1;
var Tx t with
    this.oi = &oi;
end;
escape 1;
]],
    ref = 'line 11 : invalid attribution : variable "oi" is already bound',
}
Test { [[
class Tx with
    output/input:
        var& int oi;
do
    var int oi_=0;
    this.oi = &oi_;
end

var int oi = 1;
var Tx t with
    this.oi = &oi;
end;
escape 1;
]],
    ref = 'line 11 : invalid attribution : variable "oi" is already bound',
}

Test { [[
class Tx with
    input:
        var& int i;

    output:
        var& int o;

    input/output:
        var& int io;

    output/input:
        var& int oi;
do
    var int o_  = 1;
    var int io_ = 1;
    var int oi_ = 1;

    o  = &o_;
    oi = &oi_;

    o  = 1;
    io = 1;
    oi = 1;

    if io_ and o and io and oi then end;
    if this.o and this.io and this.oi then end;
end

var int i  = 1;
var int io = 1;
var int oi = 1;
var Tx t with
    this.i  = &i;
    this.io = &io;
end;
t.i  = 1;
t.io = 1;
t.oi = 1;
escape t.o+t.io+t.oi;
]],
    run = 3,
}
Test { [[
class Tx with
    input:
        var& int i;

    output:
        var& int o;

    input/output:
        var& int io;

    output/input:
        var& int oi;
do
    var int o_  = 1; if o_ then end;
    var int io_ = 1; if io_ then end;
    var int oi_ = 1;

    this.o  = &o_;
    this.oi = &oi_;

    this.o  = 1;
    this.io = 1;
    this.oi = 1;
end

var int i  = 1;
var int io = 1;
var int oi = 1;
var Tx t with
    this.i  = &i;
    this.io = &io;
end;
t.i  = 1;
t.io = 1;
t.oi = 1;
escape t.o+t.io+t.oi;
]],
    run = 3,
}

Test { [[
class Tx with
    input:
        var int i=1;

    output:
        var int o=1;

    input/output:
        var int io=1;

    output/input:
        var int oi=1;
do
end
var Tx t with
end;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    input/output:
        var int io;
do
end
var Tx t with
end;
escape 1;
]],
    ref = 'line 7 : missing initialization for field "io" (declared in tests.lua:3)',
}

Test { [[
class Tx with
    input:
        var int i;
do
end
var Tx t with
end;
escape 1;
]],
    ref = 'line 7 : missing initialization for field "i" (declared in tests.lua:3)',
}

Test { [[
class Tx with
    input:
        var int i;

    output:
        var int o;

    input/output:
        var int io;

    output/input:
        var int oi;
do
    this.o  = 1;
    this.oi = 1;
end
var Tx t with
    this.i  = 1;
    this.io = 1;
end;
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    input:
        var& int i;
do
end
var Tx t with
end;
escape 1;
]],
    ref = 'line 7 : missing initialization for field "i" (declared in tests.lua:3)',
}

Test { [[
class Tx with
    input:
        var& int io;
do
end
var Tx t with
end;
escape 1;
]],
    ref = 'line 7 : missing initialization for field "io" (declared in tests.lua:3)',
}

Test { [[
class Tx with
    output:
        var& int o;
do
end
escape 1;
]],
    ref = 'line 3 : uninitialized variable "o" crossing compound statement (tests.lua:1)',
}

Test { [[
class Tx with
    output/input:
        var& int oi;
do
end
escape 1;
]],
    ref = 'line 3 : uninitialized variable "oi" crossing compound statement (tests.lua:1)',
}

Test { [[
class Tx with
    input:
        var& int i;

    output:
        var& int o;

    input/output:
        var& int io;

    output/input:
        var& int oi;
do
    var int o_ = 1;
    o  = &o_;
    oi = &o_;
end
var int i=0;
var Tx t with
    this.i  = &i;
    this.io = &i;
end;
escape 1;
]],
    run = 1,
}
Test { [[
class Tx with
    input:
        var& int i;

    output:
        var& int o;

    input/output:
        var& int io;

    output/input:
        var& int oi;
do
    var int o_ = 1;
    this.o  = &o_;
    this.oi = &o_;
end
var int i=0;
var Tx t with
    this.i  = &i;
    this.io = &i;
end;
escape 1;
]],
    run = 1,
}

Test { [[
class SDL with
    input:
        var int w;
do
    var int x = w;
    if x then end
end
escape 1;
]],
    run = 1,
}

Test { [[
class SDL with
    input:
        var int w;
do
native _f;
    _f(this.w);
end
escape 1;
]],
    gcc = 'implicit declaration of function ‘f’',
}

Test { [[
class Tx with
    input:
        var int v;
    code/instantaneous Build (var int v)=>Tx;
do
    code/instantaneous Build (var int v)=>Tx do
        this.v = v;
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var int i;
do
end
var Tx t with
    var int i  = this.i;
end;
escape 1;
]],
    ref = 'line 6 : invalid access to uninitialized variable "i" (declared at tests.lua:2)',
    --mode = ' line 6 : cannot read field inside the constructor',
}

Test { [[
interface I with
output:
    var& int v;
end

class Bridger with
    var& I i;
do
    var& int v = &this.i.v;
    if v then end;
end
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    output:
        vector&[] byte name;
do
    vector[] byte name_ = [].."oi";
    this.name = &name_;
    await FOREVER;
end

var Tx t;
native/nohold _strlen;
native _char;
escape _strlen(&&t.name as _char&&);
]],
    run = 2,
}

Test { [[
interface I with
    output:
        vector&[] byte name;
end

class Tx with
    interface I;
do
    vector[] byte name_ = [].."oi";
    this.name = &name_;
    await FOREVER;
end

class U with
    var& Tx t;
do
    vector&[] byte name = &this.t.name;
end

var Tx t;
var U u with
    this.t = &t;
end;

native/nohold _strlen;
native _char;
escape _strlen(&&t.name as _char&&);
]],
    run = 2,
}

--<<< INTERFACE / BLOCKI / INPUT / OUTPUT / INPUT/OUTPUT / OUTPUT/INPUT

-->>> REQUESTS

if false then

Test { [[
output/input X (var int max)=>void;
escape 1;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
input/output X (var int max)=>void do
    if max then end;
    escape;
end
escape 1;
]],
    run = 1,
}

Test { [[
input/output [10] LINE (var int max)=>byte&&;
request LINE;
escape 1;
]],
    env = 'line 2 : arity mismatch',
    --env = 'line 2 : missing parameters on `emit´',
}

Test { [[
input/output [10] LINE (var int max)=>byte&&;
request LINE => "oi";
escape 1;
]],
    env = 'line 2 : wrong argument #2',
}

Test { [[
input/output [10] LINE (var int max)=>byte&&;
request LINE => 10;
escape 1;
]],
    props = 'line 2 : invalid `emit´',
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
output/input [10] LINE (var int max)=>byte&&;
par/or do
    request LINE => 10;
with
end
escape 1;
]],
    run = 1,
}

Test { [[
input void&& A;
do
    var void&& p;
    p = await A
        until p==null;
    var void&& p1 = p;
end
await FOREVER;
]],
    _ana = {
        isForever = true,
    },
}

Test { [[
var byte&&? ret = null;
escape ret! == null;
]],
    run = 1,
}

Test { [[
input (int, byte&&) LINE;
par do
    var byte&&? ret;
    var u8 err;
    (err, ret) = await LINE;
    if err then end;
    escape not ret?;
with
    async do
        emit LINE => (1,null);
    end
end
]],
    run = 1,
}

Test { [[
var int v = 1;
var& int? x;
if false then
    x = &v;
else
    x = &v;
end
escape x!;
]],
    run = 1,
}

Test { [[
var int? v;
if true then
    v = 1;
end
escape v!;
]],
    run = 1,
}

Test { [[
output/input [10] LINE (var int max)=>byte&&;
var byte&& ret = null;
par/or do
    var byte&&? ret1;
    var u8 err;
    (err, ret1) = request LINE => 10;
    ret := ret1!;
with
    await FOREVER;
end
escape *ret;
]],
    fin = 'line 11 : unsafe access to pointer "ret" across `await´',
    --fin = 'line 5 : invalid block for awoken pointer "ret"',
}

Test { [[
output/input [10] LINE (var int max)=>byte&&;
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
par/or do
    var byte&& ret;
    var u8 err;
    (err, ret) = request LINE => 10;
with
end
escape 1;
]],
    env = 'line 8 : payload "ret" must be an option type',
}

Test { [[
output/input [10] LINE (var int max)=>byte&&;
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
par/or do
    var byte&&? ret;
    var u8 err;
    (err, ret) = request LINE => 10;
    if err and ret? then end;
with
end
escape 1;
]],
    run = 1,
}

Test { [[
input/output [10] LINE (var int max)=>byte&&;
request LINE;
escape 1;
]],
    env = 'line 2 : arity mismatch',
    --env = 'line 2 : missing parameters on `emit´',
}

Test { [[
input/output [10] LINE (var int max)=>byte&&;
request LINE => "oi";
escape 1;
]],
    env = 'line 2 : wrong argument #2',
}

Test { [[
input/output [10] LINE (var int max)=>byte&&;
request LINE => 10;
escape 1;
]],
    props = 'line 2 : invalid `emit´',
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
output/input [10] LINE (var int max)=>byte&&;
par/or do
    request LINE => 10;
with
end
escape 1;
]],
    run = 1,
}

Test { [[
input (int, byte&&) LINE;
var u8 err;
var u8? ret;
(err, ret) = await LINE;
escape 1;
]],
    env = 'line 4 : wrong argument #2',
}

Test { [[
output/input [10] LINE (var int max)=>byte&&;
var u8 err;
var u8? ret;
(err, ret) = request LINE => 10;
escape 1;
]],
    env = 'line 4 : wrong argument #3',
    --env = 'line 3 : invalid attribution (u8 vs byte&&)',
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
output/input [10] LINE (var int max)=>int;
par/or do
    var u8 err;
    var int? ret;
    (err, ret) = request LINE => 10;
    if err and ret? then end;
with
end
escape 1;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
output/input [10] LINE (var int)=>int do
    escape 1;     // missing <int "id">
end
par/or do
    var u8 err, ret;
    (err, ret) = request LINE => 10;
with
end
escape 1;
]],
    parser = 'line 4 : after `int´ : expected type modifier or `;´',
    --adj = 'line 4 : missing parameter identifier',
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
output/input [10] LINE (var int max)=>int do
    escape 1;
end
par/or do
    var u8 err;
    var u8? ret;
    (err, ret) = request LINE => 10;
with
end
escape 1;
]],
    props = 'line 4 : invalid `emit´',
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
input/output [10] LINE (var int max)=>int do
    escape 1;
end
par/or do
    var u8 err;
    var u8? ret;
    (err, ret) = request LINE => 10;
with
end
escape 1;
]],
    props = 'line 10 : invalid `emit´',
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
input/output [10] LINE (var int max)=>int do
    escape 1;
end
escape 1;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
var int ret = 0;
input/output [10] LINE (var int max)=>int do
    ret = 1;
end
escape ret;
]],
    locs = 'line 6 : internal identifier "ret" is not declared',
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [10] LINE (var int max)=>int do
native _V;
        _V = 10;
        escape 1;
    end
    await 1s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,10);
        emit 1s;
    end
end
]],
    run = 11,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [10] LINE (var int max)=>int do
native _V;
        _V = max;
    end
    await 1s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,10);
        emit 1s;
    end
end
]],
    run = 11,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [10] LINE (var int max)=>int do
native _V;
        _V = _V + max;
    end
    await 1s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,10);
        emit LINE_REQUEST => (2,20);
        emit LINE_REQUEST => (3,30);
        emit 1s;
    end
end
]],
    run = 61,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
    end
    input/output [2] LINE (var int max)=>int do
        await 1s;
    end
    await 1s;
    escape 1;
with
    async do
        emit LINE_REQUEST => (1,10);
        emit LINE_REQUEST => (1,10);
        emit 1s;
    end
end
]],
    run = 1,
}
Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    await 1s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,10);
        emit LINE_REQUEST => (1,10);
        emit 1s;
    end
end
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}
Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [2] LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    await 1s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,10);
        emit LINE_REQUEST => (2,20);
        emit LINE_REQUEST => (3,30);
        emit 1s;
    end
end
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [2] LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    await 2s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,10);
        emit LINE_REQUEST => (2,20);
        emit LINE_REQUEST => (3,30);
        emit 2s;
    end
end
]],
    _ana = {
        acc = 1,
    },
    run = 31,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [2] LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    await 3s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,11);
        emit LINE_REQUEST => (2,22);
        emit LINE_REQUEST => (3,30);
        emit 1s;
        emit LINE_REQUEST => (4,13);
        emit LINE_REQUEST => (5,24);
        emit LINE_REQUEST => (6,30);
        emit 2s;
    end
end
]],
    _ana = {
        acc = 1,
    },
    run = 71,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [1] LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    await 3s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,11);
        emit LINE_REQUEST => (2,22);
        emit LINE_REQUEST => (3,30);
        emit 1s;
        emit LINE_REQUEST => (4,13);
        emit LINE_REQUEST => (5,24);
        emit LINE_REQUEST => (6,30);
        emit 2s;
    end
end
]],
    _ana = {
        acc = 1,
    },
    run = 25,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [0] LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    await 3s;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,11);
        emit LINE_REQUEST => (2,22);
        emit LINE_REQUEST => (3,30);
        emit 1s;
        emit LINE_REQUEST => (4,13);
        emit LINE_REQUEST => (5,24);
        emit LINE_REQUEST => (6,30);
        emit 2s;
    end
end
]],
    _ana = {
        acc = 1,
    },
    run = 1,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [10] LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    input void A;
    await A;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,11);
        emit LINE_REQUEST => (2,22);
        emit LINE_CANCEL => 1;
        emit 3s;
        emit A;
    end
end
]],
    run = 23,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [10] LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    input void A;
    await A;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,11);
        emit LINE_REQUEST => (2,22);
        emit LINE_CANCEL => 2;
        emit 3s;
        emit A;
    end
end
]],
    run = 12,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
        int V = 0;
    end
    input/output [10] LINE (var int max)=>int do
        await 1s;
native _V;
        _V = _V + max;
    end
    input void A;
    await A;
    escape _V+1;
with
    async do
        emit LINE_REQUEST => (1,11);
        emit LINE_REQUEST => (2,22);
        emit LINE_CANCEL => 2;
        emit LINE_CANCEL => 1;
        emit 3s;
        emit A;
    end
end
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
    int V = 0;
end
output/input LINE (var int max)=>int;
var int? v   = 0;
var int err = 0;
(err,v) = request LINE=>10;
escape err;
]],
    run = 1,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,0)
        int V = 0;
    end
    output/input LINE (var int max)=>int;
    var int? v  = 0;
    var int err = 0;
    (err,v) = request LINE=>10;
    escape v!+err;
with
    async do
        emit LINE_RETURN => (1,1,10);
    end
end
]],
    run = 1,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,0)
        int V = 0;
    end
    output/input LINE (var int max)=>int;
    var int? v;
    var int err = 0;
    (err,v) = request LINE=>10;
    escape v!+err;
with
    async do
        emit LINE_RETURN => (1,1,10);
    end
end
]],
    run = '10] runtime error: invalid tag',
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,0)
        int V = 0;
    end
    output/input LINE (var int max)=>int;
    var int? v  = 0;
    var int err = 0;
    par/or do
        (err,v) = request LINE=>10;
    with
        await 5s;
        escape 999;
    end
    escape v!+err;
with
    async do
        emit LINE_RETURN => (1,1,10);
        emit 5s;
    end
end
]],
    run = 1,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,0)
        int V = 0;
    end
    output/input LINE (var int max)=>int;
    var int? v  = 0;
    var int err = 0;
    par/or do
        (err,v) = request LINE=>10;
    with
        await 5s;
        escape 999;
    end
    escape v!+err;
with
    async do
        emit LINE_RETURN => (2,1,10);
        emit 5s;
    end
end
]],
    run = 999,
}

Test { [[
par do
    native do
        ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,0)
        int V = 0;
    end
    output/input LINE (var int max)=>int;
    var int? v  = 0;
    var int err = 0;
    par/or do
        (err,v) = request LINE=>10;
    with
        await 5s;
        escape 999;
    end
    escape v!+err;
with
    async do
        emit LINE_RETURN => (2,1,10);
        emit 4s;
        emit LINE_RETURN => (1,0,-1);
        emit 1s;
    end
end
]],
    run = -1,
}

Test { [[
output byte[] OUT;
vector[] byte xxx = [] .. "1234567890";
emit OUT => []..xxx;
escape 1;
]],
    parser = 'line 1 : after `byte´ : expected type modifier or external identifier',
    --env = 'line 1 : invalid event type',
}

Test { [[
output byte[]&& && OUT;
]],
    parser = 'line 1 : after `byte´ : expected type modifier or external identifier',
    --env = 'line 1 : invalid event type',
}
Test { [[
output byte[]& && OUT;
]],
    parser = 'line 1 : after `byte´ : expected type modifier or external identifier',
    --env = 'line 1 : invalid event type',
}
Test { [[
class Tx with do end
output Tx OUT;
]],
    env = 'line 2 : invalid event type',
}
Test { [[
class Tx with do end
output Tx&& OUT;
]],
    env = 'line 2 : invalid event type',
}

-- TODO: dropped support for i/o vectors

Test { [[
input byte[] IN;
var int ret = 0;
par/and do
    vector[] byte&& vec = await IN;
    ret = $vec;
with
    async do
        vector[] byte vec = [1,2,3,4,5];
        emit IN => &&vec;
    end
end
escape $vec;
]],
    parser = 'line 1 : after `byte´ : expected type modifier or external identifier',
    --env = 'line 1 : invalid event type',
}

Test { [[
native do
    ##define ceu_out_emit_OUT(x) (x->_1->nxt)
end
output int[]&& OUT;
vector[] int xxx = [1,2,3,4,5];
var int ret = emit OUT => &&xxx;
escape ret;
]],
    run = 5,
    todo = 'TODO: dropped support for vector i/o',
}

Test { [[
native do
    ##define ceu_out_emit_OUT(x) (x->_1->nxt)
end
output byte[]&& OUT;
vector[] byte xxx = [] .. "1234567890";
var int ret = emit OUT => &&xxx;
escape ret;
]],
    run = 10,
    todo = 'TODO: dropped support for vector i/o',
}

Test { [[
input byte[]&& IN;
var int ret = 0;
par/and do
    vector[] byte&& vec = await IN;
    ret = $*vec;
with
    async do
        vector[] byte vec = [1,2,3,4,5];
        emit IN => &&vec;
    end
end
escape ret;
]],
    run = 5,
    todo = 'TODO: dropped support for vector i/o',
}

Test { [[
native do
    ##define ceu_out_emit_OUT(x) (x->_2->nxt + x->has_vector)
end
output (int,int[]&&,int) OUT;
vector[] int xxx = [1,2,3,4,5];
var int ret = emit OUT => (0,&&xxx,1);
escape ret;
]],
    env = 'line 4 : invalid event type : vector only as the last argument',
    todo = 'TODO: dropped support for vector i/o',
}

Test { [[
native do
    ##define ceu_out_emit_OUT(x) (x->_3->nxt + x->vector_offset)
end
output (int,int,int[]&&) OUT;
vector[] int xxx = [1,2,3,4,5];
var int ret = emit OUT => (0,1,&&xxx);
escape ret;
]],
    opts = '--tuple-vector',
    run = 21,
    todo = 'TODO: dropped support for vector i/o',
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) ceu_out_event_F(a,b,c,d)
    int ceu_out_event_F (tceu_app* app, int id_out, int len, byte* data) {
        u8 vector_offset = (((u8*)data)[0]);
        tceu_vector** v = (tceu_vector**)(data + vector_offset);
        escape (*v)->nxt;
    }
end
output (int,int,int[]&&) OUT;
vector[] int xxx = [1,2,3,4,5];
var int ret = emit OUT => (0,1,&&xxx);
escape ret;
]],
    opts = '--tuple-vector',
    run = 5,
    todo = 'TODO: dropped support for vector i/o',
}

Test { [[
output/input SERIAL_CHAR (void)=>byte;
escape 1;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,0)
end

input void OS_START;
output/input SERIAL_CHAR (void)=>byte;

par/or do
    var int err;
    var byte? v;
    (err,v) = request SERIAL_CHAR;
    if err and v? then end;
with
end

escape 1;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,0)
end
input/output SERIAL_CHAR (void)=>byte do
    escape 'a';
end
escape 1;
]],
    run = 1,
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) ceu_out_event_F(a,b,c,d)
    int V = 0;
    int ceu_out_event_F (tceu_app* app, int id_out, int len, byte* data) {
        {
            u8 vector_offset = (((u8*)data)[0]);
            if (vector_offset > 0) {
                tceu_vector* v = *((tceu_vector**)(data + vector_offset));
                V = v->nxt;
            }
        }
        escape 1;
    }
end

input/output PING_PONG (var int x)=>byte[]&& do
    vector[] byte ret = [].."Pong ";
    native/nohold _printf;
native _char;
    _printf("%s\n", (_char&&)&&ret);
    escape &&ret;
end
async do
    emit PING_PONG_REQUEST => (0,1);
end
native _V;
escape _V;
]],
    run = 5,
    opts = '--tuple-vector',
    todo = 'TODO: dropped support for vector i/o',
}

Test { [[
output/input PING_PONG (var int x)=>byte[]&&;
vector[] byte&&? ret;
par/and do
    var int i,err;
    (i,err,ret) = await PING_PONG_RETURN;
    native/nohold _printf;
native _char;
    _printf("%s\n", (_char&&)ret!);
    if i and err then end;
with
    async do
        vector[] byte str = [].."END: 10 0";
        emit PING_PONG_RETURN => (0,0,&&str);
    end
end
escape 1;
]],
    run = 10,
    todo = 'TODO: dropped support for vector i/o',
}

Test { [[
native/plain _info;
pre native do
    int V = 0;
    typedef struct info {
        int8_t i1;
        uint16_t i2;
    } info;
end

native do
    ##define ceu_out_emit(a,b,c,d) ceu_sys_output_handler(a,b,c,d)
    int ceu_sys_output_handler(tceu_app* app, int evt_id, int evt_sz, void* evt_buf) {
        tceu__int__u8__info_h* k;
        switch (evt_id) {
            case CEU_OUT_TEST_RETURN:
                k = (tceu__int__u8__info_h*)evt_buf;
                V = V + k->_2;
            break;
        }
        escape 1;
    }
end

input/output [10] TEST (var u16 t)=>_info&& do
    var _info i = _info(42,89);
    escape &&i;
end

async do
    emit TEST_REQUEST => (0,0);
end

native _V;
escape _V;
]],
    run = 5,
}
Test { [[
native/plain _info;
pre native do
    int V = 0;
    typedef struct info {
        int8_t i1;
        uint16_t i2;
    } info;
end

native do
    ##define ceu_out_emit(a,b,c,d) ceu_sys_output_handler(a,b,c,d)
    int ceu_sys_output_handler(tceu_app* app, int evt_id, int evt_sz, void* evt_buf) {
        tceu__int__u8__info_h* k;
        switch (evt_id) {
            case CEU_OUT_TEST_RETURN:
                k = (tceu__int__u8__info_h*)evt_buf;
printf("RET %p %d\n", evt_buf, k->_2);
                V = V + k->_2;
            break;
        }
        escape 1;
    }
end

input/output [10] TEST (var u16 t)=>_info&& do
    var _info i = _info(42,89);
    await 1s;
    escape &&i;
end

async do
    emit TEST_REQUEST => (0,0);
end

await 2s;

native _V;
escape _V;
]],
    run = {['~>1s;~>2s']=2},
}

Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,0)
    int V = 0;
end

class Test with
    var u8 k;
do
    await FOREVER;
end

interface Global with
    var Test&&? ptr;
end

var Test t with
    this.k = 5;
end;
var Test&&? ptr = &&t;

input/output RESOURCE [10] (void)=>void do
native _V;
    _V = global:ptr!:k;
end

async do
    emit RESOURCE_REQUEST => 0;
end

escape _V;
]],
    run = 5,
}

end

--<<< REQUESTS

-->>> DATA INI

-- ADTs used in most examples below
DATA = [[
// C-like struct
data Pair with
    var int x;
    var int y;
end

// "Nullable pointer"
data Opt;
data Nothing is Opt;
data Ptr is Opt with
    var void&& v;
end

// List (recursive type)
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end





























// 50 lines
]]

-- STATIC ADTs

--[==[
-- HERE:
]==]

-- data type identifiers must start with an uppercase
Test { [[
data t with
    var int x;
end
escape 1;
]],
    -- TODO: better error message
    parser = 'line 1 : after `data´ : expected abstraction identifier'
}
Test { [[
data Tx with
    var int x;
end
escape 1;
]],
    wrn = true,
    run = 1,
}

-- data type identifiers cannot clash with interface/classe identifiers
Test { [[
data Tx with
    var int x;
end
interface Tx with
end
escape 1;
]],
    wrn = true,
    env = 'line 4 : top-level identifier "Tx" already taken',
}
Test { [[
interface Tx with
end
data Tx with
    var int x;
end
escape 1;
]],
    wrn = true,
    env = 'line 3 : top-level identifier "Tx" already taken',
}
Test { [[
data Tx with
    var int x;
end
data Tx with
    var int y;
end
escape 1;
]],
    tops = 'line 4 : identifier "Tx" is already declared (tests.lua : line 1)',
}
Test { [[
class Tx with
do
end
interface Tx with
end
escape 1;
]],
    env = 'top-level identifier "Tx" already taken',
}

Test { [[
data Dx with
    var int x;
end
class C with
    var Dx d = Dx(200);
do
end
var C c;
escape c.d.x;
]],
    run = 200,
}

Test { [[
data Opt;
data OptNIL is Opt;
data OptPTR is Opt with
    var void&& v;
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
data OptNIL is;
]],
    parser = 'line 1 : after `is´ : expected abstraction identifier',
}

Test { [[
data OptNIL is with
end
]],
    parser = 'line 1 : after `is´ : expected abstraction identifier',
}

Test { [[
data OptNIL with
end
]],
    parser = 'line 1 : after `with´ : expected `var´ or `vector´ or `pool´ or `event´',
}

Test { [[
data Opt;
data OptNIL is Opt_;
escape 1;
]],
    tops = 'line 2 : abstraction "Opt_" is not declared',
}

-- recursive ADTs must have a base case
Test { [[
data Opt;
data OptPTR is Opt with
    var void&& v;
end
escape 1;
]],
    wrn = true,
    adt = 'line 1 : invalid recursive base case : no parameters allowed',
}

-- the base case must appear first
Test { [[
data Opt;
data OptPTR is Opt with
    var void&& v;
end
data OptNIL is Opt;
escape 1;
]],
    wrn = true,
    adt = 'line 1 : invalid recursive base case : no parameters allowed',
}

-- the base must not have fields
Test { [[
data Opt;
data OptNIL is Opt with
    var int x;
end
data OptPTR is Opt with
    var void&& v;
end
escape 1;
]],
    wrn = true,
    adt = 'line 1 : invalid recursive base case : no parameters allowed',
}

Test { [[
data Opt;
data Nothing is Opt;

data Opt1;
data Nothing is Opt1;

escape 1;
]],
    tops = 'line 5 : identifier "Nothing" is already declared (tests.lua : line 2)',
}

-->>> DATA/EVENTS

Test { [[
data Ddd with
    var int xxx;
    event void e;
end

var Ddd d = Ddd(1);

par/and do
    await d.e;
with
    await 1s;
    emit d.e;
end

d.xxx = d.xxx + 2;
escape d.xxx;
]],
    run = { ['~>1s']=3 },
}

--<<< DATA/EVENTS

-->>> MISC

Test { [[
data SDL_Rect with
    var int x,y,w,h;
end

var SDL_Rect rect;
var SDL_Rect r = rect;

escape r.x+r.y+r.w+r.h;
]],
    ref = 'line 6 : invalid access to uninitialized variable "rect" (declared at tests.lua:5)',
}
Test { [[
data SDL_Rect with
    var int x,y,w,h;
end

var SDL_Rect rect = SDL_Rect(1,2,3,4);
var SDL_Rect r = rect;

escape r.x+r.y+r.w+r.h;
]],
    run = 10,
}
Test { [[
data Ball with
    var int x, y;
    var int radius;
end

var Ball ball = Ball(130,130,8);
escape ball.x + ball.y + ball.radius;
]],
    run = 268,
}

Test { [[
data Ball with
    var float x;
    var float y;
    var float radius;
end

var Ball ball = Ball(130,130,8);

native _add;
native do
    int add (s16 a, s16 b, s16 c) {
        escape a + b + c;
    }
end

escape _add(ball.x, ball.y, ball.radius);
]],
    run = 268,
}

Test { [[
do
    data Ball1 with
        var float x;
        var float y;
        var float radius;
    end
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
data Ball1 with
    var float x;
end
do
end
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
native do
    int add (int a, int b, int c) {
        escape a + b + c;
    }
end

var int sum = 0;
do
    data Ball1 with
        var float x;
        var float y;
        var float radius;
    end
    var Ball1 ball = Ball1(130,130,8);
native _add;
    sum = sum + _add(ball.x, ball.y, ball.radius);
end

do
    data Ball2 with
        var float x;
        var float y;
        var float radius;
    end
    var Ball2 ball = Ball2(130,130,8);
    sum = sum + _add(ball.x, ball.y, ball.radius);
end

escape sum;
]],
    run = 536,
}

Test { [[
data Tx with
    var int x;
end

code/instantaneous Fx (void)=>Tx do
    var Tx t = Tx(10);
    escape t;
end

var Tx t = Fx();
escape t.x;
]],
    wrn = true,
    run = 10,
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end
escape 1;
]],
    wrn = true,
    run = 1,
    --env = 'line 6 : undeclared type `List´',
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end
var List l = ListCONS(1,
               ListCONS(2,
                   ListNIL()));
escape 1;//((l as Cons).tail) as Cons).@head@;
]],
    adt = 'line 9 : invalid constructor : recursive data must use `new´',
    --env = 'line 9 : types mismatch (`List´ <= `List&&´)',
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end
var List l = new ListCONS(1,
                  ListCONS(2,
                   ListNIL()));
escape 1;//(((l as Cons).tail) as Cons).@head@;
]],
    --env = 'line 9 : types mismatch (`List´ <= `List&&´)',
    --adt = 'line 9 : invalid attribution : must assign to recursive field',
    adt = 'line 7 : invalid attribution : not a pool',
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end
var List&& l = ListCONS(1,
                ListCONS(2,
                    ListNIL()));
escape 1;//((l as Cons).tail) as Cons).@head@;
]],
    --env = 'line 9 : types mismatch (`List&&´ <= `List´)',
    adt = 'line 7 : invalid constructor : recursive data must use `new´',
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end

var List&& l = new ListCONS(1,
                   ListCONS(2,
                    ListNIL()));
escape 0;//((l as Cons).tail) as Cons).@head@;
]],
    --env = 'line 9 : types mismatch (`List&&´ <= `List´)',
    --adt = 'line 9 : invalid constructor : recursive data must use `new´',
    --adt = 'line 9 : invalid attribution : must assign to recursive field',
    adt = 'line 8 : invalid attribution : not a pool',
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end

pool[10] List l;
l = ListCONS(1,
        ListCONS(2,
            ListNIL()));

escape 0;//((l as Cons).tail) as Cons).@head@;
]],
    adt = 'line 9 : invalid constructor : recursive data must use `new´',
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end

pool[10] List lll;
escape lll is ListNIL;
]],
    wrn = true,
    run = 1,
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end

pool[10] List lll;
escape (lll is ListCONS) + 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
native do
    ##ifndef CEU_ADTS_NEWS_POOL
    ##error bug found
    ##endif
end

data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end

pool[10] List lll = new ListCONS(1, ListNIL());
escape (lll as ListCONS).head;
]],
    wrn = true,
    run = 1,
}

Test { [[
native do
    ##ifndef CEU_ADTS_NEWS_POOL
    ##error bug found
    ##endif
end

data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end

pool[10] List lll = new ListCONS(1, ListNIL());
escape lll.head;
]],
    env = 'TODO: no head in lll',
}

Test { [[
data List;
data ListNIL is List;
data ListCONS is List with
    var int  head;
    var List tail;
end

pool[10] List lll;
lll = new ListCONS(1,
            ListCONS(2,
                ListNIL()));
escape ((lll as ListCONS).tail as ListCONS).head;
]],
    run = 2,
}

Test { [[
data Stack;
data StackEMPTY;
data StackNONEMPTY with
    var Stack&& nxt;
end

pool[] Stack xxx = new StackNONEMPTY(
                    StackNONEMPTY(xxx));

escape 1;
]],
    wrn = true,
    env = 'line 10 : invalid constructor : recursive field "NONEMPTY" must be new data',
}

Test { [[
data Split;
data SplitHORIZONTAL is Split;
data SplitVERTICAL   is Split;

data Grid;
data GridEMPTY;
data GridSPLIT with
    var Split dir;
    var Grid  one;
    var Grid  two;
end

pool[] Grid g;
g = new GridSPLIT(SplitHORIZONTAL(), GridEMPTY(), GridEMPTY());

escape ((g as GridSPLIT).one is GridEMPTY) + ((g as GridSPLIT).two is GridEMPTY) + ((g as GridSPLIT).dir is SplitHORIZONTAL);
]],
    wrn = true,
    run = 3,
}

Test { [[
data Split;
data SplitHORIZONTAL is Split;
data SplitVERTICAL   is Split;

data Grid with
    var Split dir;
end

var Grid g1 = Grid(SplitHORIZONTAL());
var Grid g2 = Grid(SplitVERTICAL());

escape (g1.dir is SplitHORIZONTAL) + (g2.dir is SplitVERTICAL);
]],
    run = 2,
}

Test { [[
data Split;
data SplitHORIZONTAL is Split;
data SplitVERTICAL   is Split;

data Grid;
data GridEMPTY;
data GridSPLIT with
    var Split dir;
    var Grid  one;
    var Grid  two;
end

pool[5] Grid g = new GridSPLIT(
                    SplitHORIZONTAL(),
                    GridSPLIT(
                        SplitVERTICAL(),
                        GridEMPTY(),
                        GridEMPTY()));

escape 1;
]],
    env = 'line 13 : arity mismatch',
}

Test { [[
data Split;
data SplitHORIZONTAL is Split;
data SplitVERTICAL   is Split;

data Grid;
data GridEMPTY;
data GridSPLIT with
    var Split dir;
    var Grid  one;
    var Grid  two;
end

pool[5] Grid g;
g = new GridSPLIT(
            SplitHORIZONTAL(),
            GridSPLIT(
                SplitVERTICAL(),
                GridEMPTY(),
                GridEMPTY()),
            GridEMPTY());

escape 1;
]],
    run = 1,
}

Test { [[
data Split;
data SplitHORIZONTAL is Split;
data SplitVERTICAL   is Split;

data Grid;
data GridEMPTY;
data GridSPLIT with
    var Split dir;
    var Grid  one;
    var Grid  two;
end

pool[] Grid g = new GridSPLIT(
                    SplitHORIZONTAL(),
                    GridEMPTY(),
                    GridEMPTY());

escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
data Dx with
    var int x;
end
class C with
    var Dx d = Dx(200);
do
end
var C c;
escape c.d.x;
]],
    run = 200,
}

Test { [[
class Tx with
    var& float v;
do
    await FOREVER;
end

data Dx with
    var float v;
end

var Dx d;
var Tx t with
    this.v = &d.v;   // 13
end;
d = Dx(1);

escape t.v;
]],
    --ref = 'line 11 : uninitialized variable "d" crossing compound statement (tests.lua:12)',
    ref = 'line 13 : invalid access to uninitialized variable "d" (declared at tests.lua:11)',
}

Test { [[
class Tx with
    var& float v;
do
    await FOREVER;
end

data Dx with
    var float v;
end

var Dx d;
var Tx _ with
    this.v = &d.v;   // 13
end;

escape 1;
]],
    --ref = 'line 11 : uninitialized variable "d" crossing compound statement (tests.lua:12)',
    ref = 'line 13 : invalid access to uninitialized variable "d" (declared at tests.lua:11)',
}

Test { [[
class Tx with
    var& float v;
do
    await FOREVER;
end

data Dx with
    var float v;
end

var Tx _ with
    var Dx d = Dx(1);
    this.v = &d.v;
end;

escape 1;
]],
    --ref = 'line 13 : attribution to reference with greater scope',
    ref = 'line 13 : invalid attribution : variable "d" has narrower scope than its destination',
}

Test { [[
data Ee;
data Nothing is Ee;
data Xx is Ee with
    var int x;
end

var Ee e = Ee(1);

escape 1;
]],
    wrn = true,
    env = 'line 7 : union data constructor requires a tag',
}

Test { [[
data Dx with
    var int x;
end

data Ee;
data Nothing is Ee;
data Xx is Ee with
    var& Dx d;
end

var Dx d = Dx(10);
var Ee e = Xx(&d);

escape (e as Xx).d.x;
]],
    wrn = true,
    run = 10,
}

Test { [[
data Dx with
    var int x;
end

data Ee;
data Nothing is Ee;
data Xx is Ee with
    var& Dx d;
end

var Ee e;    // TODO: should bind here
do
    var Dx d = Dx(1);
    (e as Xx).d = &d;
end

escape 1;//e.Xx.d.x;
]],
    wrn = true,
    ref = 'line 11 : uninitialized variable "e" crossing compound statement (tests.lua:14)',
}
Test { [[
data Dx with
    var int x;
end

data Ee;
data Nothing is Ee;
data Xx is Ee with
    var& Dx d;
end

    var Dx d = Dx(1);
var Ee e = Xx(&d);
    (e as Xx).d = &d;

escape (e as Xx).d.x;
]],
    wrn = true,
    run = 1,
}
Test { [[
data Dx with
    var int x;
end

data Ee;
data Nothing is Ee;
data Xx is Ee with
    var& Dx d;
end

var Ee e = Ee(null);
    var Dx d = Dx(10);
    (e as Xx).d = &&d;

escape (e as Xx).d:x;
]],
    wrn = true,
    run = 10,
}

Test { [[
data Ball with
    var int x;
end

data Leaf;
data Nothing is Leaf;
data Tween is Leaf with
    var& Ball ball;
end

class LeafHandler with
    var& Leaf leaf;
do
    var& Ball ball = &(leaf as Tween).ball;
    escape ball.x;
end

var Ball ball = Ball(10);
var Leaf leaf = Tween(&ball);

var int x = do LeafHandler with
                this.leaf = &leaf;
            end;

escape x;
]],
    wrn = true,
    run = 10,
}

Test { [[
data Dx;
data Nil is Dx;
data Rec is Dx with
    var Dx r1;
    var Dx r2;
end

pool[] Dx ds = new Rec(
                    Rec(Nil(),Nil()),
                    Nil());

par/or do
    await (ds as Rec).r1;
with
    (ds as Rec).r1 = new Nil();
end

escape 1;
]],
    _ana = {acc=true},
    run = 1,
}

Test { [[
data Tree;
data Nil is Tree;
data Node is Tree with
    var int   v;
    var Tree  left;
    var Tree  right;
end

pool[3] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

class Sum with
    var int&& v;
do
    await FOREVER;
end

class Body with
    pool&[]  Body bodies;
    var   Tree&&   n;
    var&   Sum    sum;
do
    watching *n do
        if *n is Node then
            *this.sum.v = *this.sum.v + (*n as Node).v;
            spawn Body in this.bodies with
                this.bodies = &bodies;
                this.n      = && (*n as Node).left;
                this.sum    = &sum;
            end;
        end
    end
end

var int v = 0;
var Sum sum with
    this.v = &&v;
end;

pool[7] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&tree;
    this.sum    = &sum;
end;

escape v;
]],
    fin = 'line 29 : unsafe access to pointer "v" across `class´ (tests.lua : 22)',
}

Test { [[
data Vector3f with
    var float x, y, z;
end

class SurfaceBackground with
    var& _WorldObjs__SurfaceBackground me;
do
    code/instantaneous Set_pos (var _Vector3f&& p)=>void do
        this.me.pos = Vector3f(p:x, p:y, p:z);
    end
end
]],
    adt = 'line 9 : invalid attribution : destination is not a "data" type',
}

Test { [[
data Dx with
    var int x;
    var int y;
end

var Dx d1 = Dx(10,10);
var Dx d2 = d1;
d2.y = 20;

escape d1.x + d2.x + d1.y + d2.y;
]],
    run = 50,
}

Test { [[
data Dx with
    var int  x;
    var& int y;
end

var int v = 10;
var Dx d1 = Dx(10,&v);
var Dx d2 = d1;
d2.y = 20;

escape d1.x + d2.x + d1.y + d2.y;
]],
    run = 60,
}

-- << ADT : MISC

-- USE DATATYPES DEFINED ABOVE ("DATA")

-- simple test
Test { DATA..[[
escape 1;
]],
    wrn = true,
    run = 1,
}

-- constructors
Test { DATA..[[
var Pair p1 = Pair(1,2);        /* struct, no tags */
var Opt  o1 = Nothing();        /* unions, explicit tag */
var Opt  o2 = Ptr(&&p1);
pool[] List l1;
l1 = new Nil();       /* recursive union */
pool[] List l2 = new Cons(1, l1);
escape 1;
]],
    env = 'line 56 : invalid constructor : recursive field "Cons" must be new data',
    -- TODO-ADT-Rec-STATIC-CONSTRS
    --run = 1,
}

-- recursive fields are pointers
Test { DATA..[[
pool[] List l1 = new Nil();
pool[] List l2;
l2 = new Cons(1, l1);     /* should be &&l1 */
escape 1;
]],
    wrn = true,
    env = 'line 53 : invalid constructor : recursive field "Cons" must be new data',
}

-- constructors must specify the ADT identifier
Test { DATA..[[
var Pair p1 = (1,2);    /* vs Pair(1,2) */
escape 1;
]],
    -- TODO: better error message
    parser = 'line 51 : after `1´ : expected `(´ or `[´ or `:´ or `.´ or `?´ or `!´ or binary operator or `)´',
    --run = 1,
}
Test { DATA..[[
pool[] List l1 = new Nil();    /* vs Nil() */
escape 1;
]],
    wrn = true,
    env = 'line 51 : data "Nil" is not declared',
    --run = 1,
}

-- ADT/constructor has to be defined
Test { DATA..[[
var Pair p1 = Unknown(1,2);
escape 1;
]],
    tops = 'line 51 : abstraction "Unknown" is not declared',
}
Test { DATA..[[
var Opt  o1 = UnknownNIL();
escape 1;
]],
    tops = 'line 51 : abstraction "UnknownNIL" is not declared',
    --env = 'line 51 : data "Unknown" is not declared',
}

-- tag has to be defined
Test { DATA..[[
var Opt o1 = Unknown();
escape 1;
]],
    tops = 'line 51 : abstraction "Unknown" is not declared',
}

-- constructors have call syntax
Test { DATA..[[
var List l1 = Nil; /* vs Nil() */
escape 1;
]],
    parser = 'line 51 : after `Nil´ : expected `(´',
    --run = 1,
}

-- constructors must respect parameters
Test { DATA..[[
var Pair p1 = Pair();           /* expected (x,y) */
escape 1;
]],
    wrn = true,
    env = 'line 51 : arity mismatch',
}
Test { DATA..[[
var Pair p1 = Pair(1,null);     /* expected (int,int) */
escape 1;
]],
    wrn = true,
    env = 'line 51 : wrong argument #2',
}
Test { DATA..[[
var Opt o1 = Nothing(1);       /* expected (void) */
escape 1;
]],
    wrn = true,
    env = 'line 51 : arity mismatch',
}

-- constructors are not expressions...
Test { DATA..[[
escape Nil();
]],
    wrn = true,
    --ast = 'line 51 : invalid call',
    env = 'TODO: not a code',
    --parser = 'line 51 : after `escape´ : expected expression',
}
Test { DATA..[[
var List l;
var int v = (l==Nil());
escape v;
]],
    wrn = true,
    --ast = 'line 52 : invalid call',
    env = 'TODO: not a code',
    --parser = 'line 52 : after `==´ : expected expression',
}

-- ...but have to be assigned to a variable
Test { DATA..[[
var Opt o;
o = Nothing();
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { DATA..[[
pool[] List l;
l = new Nil();
escape 1;
]],
    wrn = true,
    run = 1,
}

-- TODO: uninitialized variables?
-- (default values)
-- structs: undefined
-- enums: undefined
-- recursive enums: base case

-- Destructors:
--  - like C
--      - use field names ("dot" notation)
--      - no support for pattern matching
--      - but type safe
--          - tags are checked

-- distinction "constructor" vs "tag check"
Test { DATA..[[
pool[] List l = new Nil();   /* call syntax: constructor */
var bool no_ = (l is Nil);     /* no-call syntax: check tag */
escape no_;
]],
    wrn = true,
    run = 1,
}
Test { DATA..[[
pool[] List l;
l = new Nil();   /* call syntax: constructor */
var bool no_ = l is Cons;    /* no-call syntax: check tag */
escape no_;
]],
    wrn = true,
    run = 0,
}

-- destructor == field access
Test { DATA..[[
var Pair p1 = Pair(1,2);
escape p1.x + p1.y;
]],
    wrn = true,
    run = 3,
}
-- tag Nil has no fields
Test { DATA..[[
pool[] List l;
escape (l as Nil).v;
]],
    wrn = true,
    env = 'line 52 : field "v" is not declared',
}
-- tag Opt.Ptr has no field "x"
Test { DATA..[[
var Opt o;
escape (o as Ptr).x;
]],
    wrn = true,
    env = 'line 52 : field "x" is not declared',
}

-- mixes Pair/Opt/List and also construcor/tag-check/destructor
Test { DATA..[[
pool[] List l1 = new Nil();
pool[] List l2 = new Cons(1, Nil());
escape 1;
]],
    wrn = true,
    run = 1,
}

Test { DATA..[[
var Pair p1 = Pair(1,2);
var Opt  o1 = Nothing();
var Opt  o2 = Ptr(&&p1);
pool[] List l1 = new Nil();
pool[] List l2;
l2 = new Cons(1, Nil());
pool[] List l3 = new Cons(1, Cons(2, Nil()));

var int ret = 0;                                // 0

ret = ret + p1.x + p1.y;                        // 3
ret = ret + (o1 is Nothing);                             // 4
ret = ret + ((o2 as Ptr).v==&&p1);                    // 5
ret = ret + (l1 is Nil);                             // 6
ret = ret + (l2 as Cons).head + ((l2 as Cons).tail is Nil);    // 8
ret = ret + (l3 as Cons).head + ((l3 as Cons).tail as Cons).head + (((l3 as Cons).tail as Cons).tail is Nil);   // 12

escape ret;
]],
    run = 12,
}

-- destructors are checked at runtime
--      v = ((l as Cons).head)
-- becomes
--      assert(l.Cons)
--      v = ((l as Cons).head)
Test { DATA..[[
pool[] List l;
l = new Nil();
escape (l as Cons).head;         // runtime error
]],
    wrn = true,
    asr = true,
    --run = 1,
}
Test { DATA..[[
pool[] List l = new Cons(2, Nil());
escape (l as Cons).head;
]],
    wrn = true,
    run = 2,
}

-- mixes everything:
Test { DATA..[[
var Pair p  = Pair(1,2);
var Opt  o1 = Nothing();
var Opt  o2 = Ptr(&&p);
pool[] List l1;
l1 = new Nil();
pool[] List l2 = new Cons(1, Nil());
pool[] List l3;
l3 = new Cons(1, Cons(2, Nil()));

var int ret = 0;            // 0

var int x = p.x;
var int y = p.y;
native _assert;
_assert(x+y == 3);
ret = ret + 3;              // 3

if o1 is Nothing then
    ret = ret + 1;          // 4
else/if o1 is Ptr then
    _assert(0);             // never reachable
end

if o2 is Nothing then
    _assert(0);             // never reachable
else/if o2 is Ptr then
    ret = ret + 1;          // 5
    _assert((o2 as Ptr).v==&&p);
end

if l1 is Nil then
    ret = ret + 1;          // 6
else/if l1 is Cons then
    _assert(0);             // never reachable
end

if l2 is Nil then
    _assert(0);             // never reachable
else/if l2 is Cons then
    _assert((l2 as Cons).head == 1);
    ret = ret + 1;          // 7
    if (l2 as Cons).tail is Nil then
        ret = ret + 1;      // 8
    else/if (l2 as Cons).tail is Cons then
        _assert(0);         // never reachable
    end
    ret = ret + 1;          // 9
end

if l3 is Nil then
    _assert(0);             // never reachable
else/if l3 is Cons then
    _assert((l3 as Cons).head == 1);
    ret = ret + 1;          // 10
    if (l3 as Cons).tail is Nil then
        _assert(0);         // never reachable
    else/if (l3 as Cons).tail is Cons then
        _assert(((l3 as Cons).tail as Cons).head == 2);
        ret = ret + 2;      // 12
        if ((l3 as Cons).tail as Cons).tail is Nil then
            ret = ret + 1;  // 13
        else/if ((l3 as Cons).tail as Cons).tail is Cons then
            _assert(0);     // never reachable
        end
        ret = ret + 1;      // 14
    end
    ret = ret + 1;          // 15
end

escape ret;
]],
    run = 15,
}

-- POINTERS
-- TODO: more discussion
--  - not an lvalue if rvalue not a constructor:
--      ptr as Cons).@tail@ = new ...             // ok
--      ptr as Cons).@tail@ = l....               // no
--      ptr as Cons).@tail@ = ptr as Cons).@tail@....   // ok
--          same prefix

-- cannot cross await statements
Test { DATA..[[
pool[] List l = new Cons(1, Nil());
var List&& p = (l as Cons).tail;
await 1s;
escape (*p as Cons).head;
]],
    wrn = true,
    adt = 'line 52 : invalid attribution : mutation : cannot mix data sources',
    --fin = 'line 54 : unsafe access to pointer "p" across `await´',
    --adt = 'line 52 : invalid attribution : value is not a reference',
}
Test { DATA..[[
pool[] List l = new Cons(1, Nil());
var List&& p = &&(l as Cons).tail;
await 1s;
escape (*p as Cons).head;
]],
    wrn = true,
    --adt = 'line 52 : mutation : cannot mix data sources',
    fin = 'line 54 : unsafe access to pointer "p" across `await´',
    --adt = 'line 52 : invalid attribution : value is not a reference',
}
Test { DATA..[[
pool[] List l = new Cons(1, Nil());
var List&& p = &&(l as Cons).tail;
await 1s;
escape (*p as Cons).head;
]],
    wrn = true,
    --adt = 'line 52 : cannot mix recursive data sources',
    fin = 'line 54 : unsafe access to pointer "p" across `await´',
}

-- COPY / MUTATION
--  - intentional feature
--  - ADTs are substitutes for enum/struct/union
--  - must be "as efficient" and with similar semantics
-- TODO: more discussion

-- linking a list: 2-1-Nil
Test { DATA..[[
pool[] List l1;
l1 = new Nil();
pool[] List l2 = new Cons(1, l1);
pool[] List l3;
l3 = new Cons(2, l2);
escape (l3 as Cons).head + ((l3 as Cons).tail as Cons).head + (((l3 as Cons).tail as Cons).tail is Nil);
]],
    wrn = true,
    --run = 4,
    env = 'line 53 : invalid constructor : recursive field "Cons" must be new data',
    -- TODO-ADT-Rec-STATIC-CONSTRS
}
Test { DATA..[[
pool[] List l3 = new Cons(2, Cons(1, Nil()));
escape (l3 as Cons).head + ((l3 as Cons).tail as Cons).head + (((l3 as Cons).tail as Cons).tail is Nil);
]],
    wrn = true,
    run = 4,
}
Test { DATA..[[
pool[] List l3 = new Cons(2, Cons(1, Nil()));
escape (l3 as Cons).head + ((l3 as Cons).tail as Cons).head + (((l3 as Cons).tail as Cons).tail is Nil);
]],
    wrn = true,
    run = 4,
}
-- breaking a list: 2-1-Nil => 2-Nil
Test { DATA..[[
pool[] List l1;
l1 = new Nil();
pool[] List l3 = new Cons(2, Cons(1, Nil()));
(l3 as Cons).tail = l1;
escape (l3 as Cons).head + ((l3 as Cons).tail is Nil);
]],
    wrn = true,
    --adt = 'line 54 : invalid attribution : value is not a reference',
    --adt = 'line 54 : invalid attribution : new reference only to pointer or alias',
    adt = 'line 54 : invalid attribution : mutation : cannot mix data sources',
    run = 3,
}
Test { DATA..[[
pool[] List l1;
l1 = new Nil();
pool[] List l3 = new Cons(2, Cons(1, Nil()));
(l3 as Cons).tail = &&l1;
escape (l3 as Cons).head + ((l3 as Cons).tail is Nil);
]],
    wrn = true,
    adt = 'line 54 : invalid attribution : destination is not a reference',
    --adt = 'line 54 : cannot mix recursive data sources',
    run = 3,
}

-- circular list: 1-1-1-...
Test { DATA..[[
pool[] List l1;
pool[] List l2;
l1 = new Nil();
l2 = new Cons(1, Nil());
l1 = l2;
escape (l1 is Cons) + (l1 as Cons).head==1;
]],
    wrn = true,
    --adt = 'line 55 : invalid attribution : value is not a reference',
    adt = 'line 55 : invalid attribution : mutation : cannot mix data sources',
    run = 2,
}
Test { DATA..[[
pool[] List l1;
pool[] List l2;
l1 = new Nil();
l2 = new Cons(1, Nil());
l1 = &&l2;
escape (l1 is Cons) + (l1 as Cons).head==1;
]],
    wrn = true,
    adt = 'line 55 : invalid attribution : destination is not a reference',
    --adt = 'line 55 : invalid attribution : new reference only to pointer or alias',
    --adt = 'line 55 : cannot mix recursive data sources',
    run = 2,
}
Test { DATA..[[
pool[] List l1 = new Nil(),
            l2 = new Cons(1, Nil());
l1 = l2;
escape (l1 is Cons) + ((l1 as Cons).head==1) + ((((l1 as Cons).tail as Cons).tail as Cons).head==1);
]],
    wrn = true,
    adt = 'line 53 : invalid attribution : value is not a reference',
    adt = 'line 53 : invalid attribution : mutation : cannot mix data sources',
    run = 3,
}

-- circular list: 1-2-1-2-...
Test { DATA..[[
pool[] List l1 = new Cons(1, Nil()),
            l2 = new Cons(2, Nil());
((l1 as Cons).tail) = l2;
escape (((l1 as Cons).head)==1) + ((((l1 as Cons).tail) as Cons).head==2) +
       (((l2 as Cons).head)==2) + ((((l2 as Cons).tail) as Cons).head==1) +
       ((((((l1 as Cons).tail) as Cons).tail as Cons).tail as Cons).head==2);
]],
    wrn = true,
    --adt = 'line 53 : invalid attribution : value is not a reference',
    adt = 'line 53 : invalid attribution : mutation : cannot mix data sources',
    run = 5,
}

Test { DATA..[[
pool[] List l1 = new Cons(1, Nil()),
            l2 = new Cons(2, Nil());
((l1 as Cons).tail) = &&l2;
escape (((l1 as Cons).head)==1) + ((((l1 as Cons).tail) as Cons).head==2) +
       (((l2 as Cons).head)==2) + ((((l2 as Cons).tail) as Cons).head==1) +
       ((((((l1 as Cons).tail) as Cons).tail as Cons).tail as Cons).head==2);
]],
    wrn = true,
    adt = 'line 53 : invalid attribution : destination is not a reference',
    --adt = 'line 53 : cannot mix recursive data sources',
    run = 5,
}

-- another circular list
Test { DATA..[[
pool[] List l1, l2;
l1 = new Cons(1, Nil());
l2 = new Cons(2, Nil());
((l1 as Cons).tail) = l2;
((l2 as Cons).tail) = l1;

escape ((l1 as Cons).head) + (((l1 as Cons).tail) as Cons).head + ((l2 as Cons).head) + (((l2 as Cons).tail) as Cons).head;
]],
    wrn = true,
    --adt = 'line 54 : invalid attribution : value is not a reference',
    --adt = 'line 54 : invalid attribution : new reference only to root',
    adt = 'line 54 : invalid attribution : mutation : cannot mix data sources',
    run = 6,
}

-- not circular
Test { DATA..[[
pool[] List l1, l2;
l1 = new Nil();
l2 = new Cons(1, Nil());
l1 = ((l2 as Cons).tail);
escape l1 is Nil;
]],
    wrn = true,
    --adt = 'line 54 : invalid attribution : value is not a reference',
    --adt = 'line 54 : invalid attribution : new reference only to pointer or alias',
    adt = 'line 54 : invalid attribution : mutation : cannot mix data sources',
    run = 1,
}

-- not circular
Test { DATA..[[
pool[] List l1, l2;
l1 = new Nil();
l2 = new Cons(1, Nil());
l1 = &&((l2 as Cons).tail);
escape l1 is Nil;
]],
    wrn = true,
    adt = 'line 54 : invalid attribution : destination is not a reference',
    --adt = 'line 54 : invalid attribution : new reference only to pointer or alias',
    --adt = 'line 54 : cannot mix recursive data sources',
    run = 1,
}

-- DYNAMIC ADTs:
--  - can only describe directed-rooted-tree
--      - no double linked lists, no circular ADTs
--  - different types for static/dynamic ADTs
--  - they can never be mixed
--  - TODO:
--      - now explicit (List vs List)
--      - in the future distinguish/create automatically/implicitly
--          - declare only List
--              - implicitly expand to List/List

-- TODO: non-recursive dynamic ADTs
--  - does it even make sense?

-- dynamic ADTs require a pool
Test { DATA..[[
pool[] List l;     // all instances reside here
escape 1;
]],
    wrn = true,
    run = 1,
}

-- the pool variable is overloaded:
--  - represents the pool
--  - represents the root of the tree
Test { DATA..[[
pool[] List l;     // l is the pool
escape l is Nil;       // l is a pointer to the root
]],
    wrn = true,
    run = 1,
}
Test { DATA..[[
pool[] List l;     // l is the pool
escape (l) is Nil;    // equivalent to above
]],
    wrn = true,
    run = 1,
}
-- the pointer must be dereferenced
Test { DATA..[[
pool[] List l;     // l is the pool
escape *l is Nil;       // "l" is not a struct
]],
    wrn = true,
    env = 'line 52 : invalid operand to unary "*"',
    --env = 'line 52 : invalid access (List[] vs List)',
}
Test { DATA..[[
pool[] List l;     // l is the pool
escape ((l as Cons).head); // "l" is not a struct
]],
    wrn = true,
    env = 'line 52 : invalid operand to unary "*"',
    --env = 'line 52 : invalid access (List[] vs List)',
}
Test { DATA..[[
pool[] List l;             // l is the pool
escape *((l as Cons).tail) is Cons;    // "((l as Cons).tail)" is not a struct
]],
    wrn = true,
    env = 'line 52 : invalid operand to unary "*"',
    --env = 'line 52 : not a struct',
}

-- the pool is initialized to the base case of the ADT
-- (this is why the base case cannot have fields and
--  must appear first in the ADT declaration)
Test { DATA..[[
pool[] List l;
escape l is Cons;      // runtime error
]],
    wrn = true,
    asr = true,
}

-- dynamic ADTs have automatic memory management
--  - similar to organisms
Test { DATA..[[
var int ret = 0;
do
    pool[] List lll;
    ret = lll is Nil;
end
// all instances in "lll" have been collected
escape ret;
]],
    wrn = true,
    run = 1,
}

-- TODO: escape analysis for instances going to outer scopes
-- TODO: mixing static/static, dynamic/dynamic, static/dynamic

-- Dynamic constructors:
--  - must use "new"
--  - the pool is inferred from the l-value
Test { DATA..[[
pool[] List l;
l = new Nil();
escape l is Nil;
]],
    wrn = true,
    run = 1,
}
Test { DATA..[[
pool[] List l = new Cons(2, Nil());
escape ((l as Cons).head);
]],
    wrn = true,
    run = 2,
}
Test { DATA..[[
pool[] List l = new Cons(1, Cons(2, Nil()));
escape ((l as Cons).head) + (((l as Cons).tail) as Cons).head + ((((l as Cons).tail) as Cons).tail is Nil);
]],
    wrn = true,
    run = 4,
}
-- wrong tag
Test { DATA..[[
pool[] List l;
l = new Nil();
escape l is Cons;
]],
    wrn = true,
    asr = true,
}
-- no "new"
Test { DATA..[[
pool[] List l;
l = Cons(2, Nil());
escape ((l as Cons).head);
]],
    wrn = true,
    adt = 'line 52 : invalid constructor : recursive data must use `new´',
    --env = 'line 52 : invalid call parameter #2 (List vs List&&)',
}
-- cannot assign "l" directly (in the pool declaration)
Test { DATA..[[
pool[] List l = new Cons(2, Nil());
escape ((l as Cons).head);
]],
    wrn = true,
    run = 2,
}
-- no dereference
Test { DATA..[[
pool[] List l;
l = new Nil();
escape l is Nil;
]],
    wrn = true,
    --env = 'line 53 : invalid access (List[] vs List)',
    run = 1,
}
Test { DATA..[[
pool[] List l;
l = new Cons(2, Nil());
escape ((l as Cons).head);
]],
    wrn = true,
    --env = 'line 53 : invalid access (List[] vs List)',
    run = 2,
}

-- static vs heap pools
--      pool[] List l;      // instances go to the heap
-- vs
--      pool[10] List l;    // 10 instances at most
-- (same as for organisms)

-- allocation fails (0 space)
-- fallback to base case (which is statically allocated in the initialization)
-- (this is also why the base case cannot have fields and
--  must appear first in the ADT declaration)
-- (
Test { DATA..[[
pool[0] List l = new Cons(2, Nil());
escape l is Nil;
]],
    wrn = true,
    run = 1,
}
Test { DATA..[[
pool[0] List l;
l = new Cons(2, Nil());
escape ((l as Cons).head);     // runtime error
]],
    wrn = true,
    asr = true,
}
-- 2nd allocation fails (1 space)
Test { DATA..[[
pool[1] List l = new Cons(2, Cons(1, Nil()));
native _assert;
_assert(((l as Cons).tail) is Nil);
escape ((l as Cons).head);
]],
    wrn = true,
    run = 2,
}
-- 3rd allocation fails (2 space)
Test { DATA..[[
pool[2] List l = new Cons(1, Cons(2, Cons(3, Nil())));
native _assert;
_assert((((l as Cons).tail) as Cons).tail is Nil);
escape ((l as Cons).head) + (((l as Cons).tail) as Cons).head + (((l as Cons).tail) as Cons).tail is Nil;
]],
    wrn = true,
    run = 4,
}

-- dereference test for static pools
-- (nothing new here)
Test { DATA..[[
pool[0] List l;
l = new Cons(2, Nil());
escape l is Nil;
]],
    wrn = true,
    --env = 'line 53 : invalid access (List[] vs List)',
    run = 1,
}

Test { [[
data Tx;
data Nil is Tx;
data Nxt is Tx with
    var int v;
    var Tx&&  nxt;
end
pool[] Tx ts;
do
    ts = new Nil();
end
escape ts is Nil;
]],
    wrn = true,
    run = 1,
}

-- Mutation in dynamic ADTs:
--  - "dropped" subtrees are automatically reclaimed:
--      l = new ...
-- becomes
--      tmp = new ...   // "new" happens before!
--      free(l)         // "free" happens after!
--      l = tmp
--  (this is why dynamic ADTs have to be a directed rooted trees)

-- 1-Nil => 2-Nil
-- 1-Nil can be safely reclaimed
Test { DATA..[[
pool[1] List l = new Cons(1, Nil());
l = new Cons(2, Nil());    // this fails (new before free)!
escape ((l as Cons).head);
]],
    wrn = true,
    asr = true,
}

Test { DATA..[[
pool[1] List l;
l = new Cons(1, Nil());
((l as Cons).tail) = new Cons(2, Nil()); // fails
escape ((l as Cons).tail) is Nil;
]],
    wrn = true,
    run = 1,
    --asr = true,
}

-- 1-2-Nil
Test { DATA..[[
pool[2] List l = new Cons(1, Nil());
((l as Cons).tail) = new Cons(2, Nil()); // fails
escape (((l as Cons).tail) as Cons).head;
]],
    wrn = true,
    run = 2,
}

-- 1-Nil => 2-Nil
-- 1-Nil can be safely reclaimed
Test { DATA..[[
pool[2] List l;
l = new Cons(1, Nil());
l = new Cons(2, Nil());    // no allocation fail
escape ((l as Cons).head);
]],
    wrn = true,
    run = 2,
}

-- 1-2-3-Nil => 1-2-Nil (3 fails)
-- 4-5-6-Nil => Nil     (all fail)
Test { DATA..[[
native _ceu_out_assert_msg;
pool[2] List l = new Cons(1, Cons(2, Cons(3, Nil())));   // 3 fails
_ceu_out_assert_msg((((l as Cons).tail) as Cons).tail is Nil, "1");
l = new Nil();
l = new Cons(4, Cons(5, Cons(6, Nil())));   // 6 fails
_ceu_out_assert_msg((((l as Cons).tail) as Cons).tail is Nil, "2");
escape (((l as Cons).tail) as Cons).head;
]],
    wrn = true,
    run = 5,
}

Test { DATA..[[
pool[2] List l = new Cons(1, Cons(2, Cons(3, Nil())));   // 3 fails
native _ceu_out_assert_msg;
_ceu_out_assert_msg((((l as Cons).tail) as Cons).tail is Nil, "1");
l = new Cons(4, Cons(5, Cons(6, Nil())));   // all fail
escape l is Nil;
]],
    wrn = true,
    run = 1,
}

-- 1-2-3-Nil => 1-2-Nil (3 fails)
-- (clear all)
-- 4-5-6-Nil => 4-5-Nil (6 fails)
Test { DATA..[[
pool[2] List l;
l = new Cons(1, Cons(2, Cons(3, Nil())));   // 3 fails
native _assert;
_assert((((l as Cons).tail) as Cons).tail is Nil);
l = new Nil();                                                // clear all
l = new Cons(4, Cons(5, Cons(6, Nil())));   // 6 fails
_assert((((l as Cons).tail) as Cons).tail is Nil);
escape ((l as Cons).head) + (((l as Cons).tail) as Cons).head + (((((l as Cons).tail) as Cons).tail is Nil));
]],
    wrn = true,
    run = 10,
}

-- Mutation in subtrees:
--  - ok: child attributed to parent
--      - parent subtree is dropped, child substitutes it
--  - no: parent attributed to child
--      - creates a cycle / makes child orphan
--      - TODO (could "swap" ?)
--  - RULE: either r-val is constructor or
--                 l-val is prefix of r-val

-- ok: child is constructor (with no previous parent)
-- Nil
-- 1-Nil
-- 1-2-Nil
Test { DATA..[[
pool[2] List l = new Cons(1, Nil());
((l as Cons).tail) = new Cons(2, Nil());
escape ((l as Cons).head) + (((l as Cons).tail) as Cons).head;
]],
    wrn = true,
    run = 3,
}
-- ok: tail is child of l
-- Nil
-- 1-2-Nil
-- 1-Nil
Test { DATA..[[
pool[2] List lll;
lll = new Cons(1, Cons(2, Nil()));
lll = (lll as Cons).tail;    // parent=child
escape (lll as Cons).head;
]],
    wrn = true,
    run = 2,
}
Test { DATA..[[
pool[2] List lll = new Cons(1, Cons(2, Nil()));
lll = (lll as Cons).tail;
(lll as Cons).tail = new Cons(3, Nil());
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { DATA..[[
pool[2] List lll;
lll = new Cons(1, Cons(2, Nil()));
lll = (lll as Cons).tail;    // parent=child
(lll as Cons).tail = new Cons(3, Cons(4, Nil()));    // 4 fails
escape (lll as Cons).head + (((lll as Cons).tail) as Cons).head + ((((lll as Cons).tail) as Cons).tail is Nil);
]],
    wrn = true,
    run = 6,
}
Test { DATA..[[
pool[2] List l = new Cons(1, Cons(2, Nil()));
l = ((l as Cons).tail);    // parent=child
((l as Cons).tail) = new Cons(3, Cons(4, Nil()));    // 4 fails
escape ((l as Cons).head) + (((l as Cons).tail) as Cons).head + ((((l as Cons).tail) as Cons).tail is Nil);
]],
    wrn = true,
    run = 6,
}

-- no: l is parent of tail
-- Nil
-- 1-2-Nil
-- 1-2-^1   (no)
Test { DATA..[[
pool[2] List l;
l = new Cons(1, Cons(2, Nil()));
((l as Cons).tail) = l;    // child=parent
escape 1;
]],
    wrn = true,
    adt = 'line 53 : cannot assign parent to child',
}

-->>> OPTION TYPES

Test { [[
data OptionInt;
data Nil1 is OptionInt;
data Some1 is OptionInt with
    var int v;
end

data OptionPtr;
data Nil2 is OptionPtr;
data Some2 is OptionPtr with
    var int&& v;
end

var int ret = 0;            // 0

var OptionInt i = Nil1();
var OptionPtr p = Nil2();
ret = ret + (i is Nil1) + (p is Nil2);  // 2

i = Some1(3);
ret = ret + (i as Some1).v;       // 5

p = Some2(&&ret);
*(p as Some2).v = *(p as Some2).v + 2;    // 7

var int v = 10;
p = Some2(&&v);
*(p as Some2).v = *(p as Some2).v + 1;

ret = ret + v;              // 18
escape ret;
]],
    run = 18,
}

Test { [[
var int? i;
if i? then end;
escape 1;
]],
    run = 1,
}

Test { [[
var int? i = 1;
escape i!;
]],
    run = 1,
}

Test { [[
var int? i;
escape not i?;
]],
    run = 1,
}

Test { [[
var int? i;
escape not i?;
]],
    run = 1,
}

Test { [[
var int v = 10;
var& int? i;
escape not i?;
]],
    --ref = 'line 3 : reference must be bounded before use',
    run = 1,
}

Test { [[
var int v = 10;
var& int? i;
escape not i?;
]],
    run = 1,
}

Test { [[
var int v = 10;
var& int? i;
escape not i?;
]],
    run = 1,
}

Test { [[
var int v = 10;
var& int? i = &v;
escape i!;
]],
    run = 10,
}

Test { [[
var int v1 = 0;
var int v2 = 1;
var& int? i = &v1;
i! = v2;
escape v1;
]],
    run = 1,
    --code = 'line 4 : invalid operand in assignment',
}

Test { [[
var int v1 = 0;
var int v2 = 1;
var& int? i = &v1;
i = v2;
escape v1;
]],
    env = 'line 4 : invalid attribution : missing `!´ (in the left) or `&´ (in the right)',
}
Test { [[
var int v1 = 0;
var int v2 = 1;
var& int? i = &v1;
i = &v2;
escape v1;
]],
    ref = 'line 4 : invalid attribution : variable "i" is already bound',
    --ref = 'line 4 : invalid attribution : l-value already bounded',
}

Test { [[
var int v1 = 0;
var int v2 = 1;
var& int? i = &v1;
i! = v2;
escape v1;
]],
    run = 1,
}

Test { [[
var int v = 10;
var& int i = &v;
escape v + i;
]],
    run = 20,
}

Test { [[
var int v = 10;
var& int? i = &v;
escape v + i!;
]],
    run = 20,
}

Test { [[
var int v1 = 10;
var int v2 =  1;
var& int? i = &v1;
i! = v2;
i! = 10;
var int ret = i!;
escape v1 + v2 + ret;
]],
    run = 21,
}

Test { [[
class Tx with
    var& int? i;
do
    var int v = 10;
    this.i! = v;
end
var Tx t;
escape t.i!;
]],
    asr = ':5] runtime error: invalid tag',
    --run = 10,
}
Test { [[
class Tx with
    var& int? i;
do
    var int v = 10;
    this.i! = v;
end
var int i = 0;
var Tx t with
    this.i = &i;
end;
escape t.i!;
]],
    --code = 'line 5 : invalid operand in assignment',
    --asr = ':5] runtime error: invalid tag',
    run = 10,
}
Test { [[
var int v = 10;
class Tx with
    var& int? i;
do
    var int v = 10;
    if v then end;
end
var Tx t with
    this.i = &v;
end;
v = v / 2;
escape t.i? + t.i! + 1;
]],
    run = 7,
}
Test { [[
class Tx with
    var& int? i;
do
    var int v = 10;
    if v then end;
end
var Tx t;
escape t.i? + 1;
]],
    run = 1,
}
Test { [[
class Tx with
    var& int? i;
do
    var int v = 10;
    if v then end;
end
var Tx t;
escape t.i!;
]],
    asr = true,
}
Test { [[
class Tx with
    var& int? i;
do
    var int v = 10;
    if v then end;
end
var int v = 1;
var Tx t with
    this.i = &v;
end;
v = 11;
escape t.i!;
]],
    run = 11,
}

Test { [[
native _SDL_Texture;
native/nohold _g;
var& _SDL_Texture? t_enemy_0, t_enemy_1;
native _f;
    do t_enemy_1 = &_f();
finalize with
    _g(&&t_enemy_1!);
end
escape 1;
]],
    gcc = 'error: unknown type name ‘SDL_Texture’',
}

Test { [[
pre native do
    typedef struct {
        int x;
    } t;
    int id (int v) {
        escape v;
    }
end
native/pure _id;

native/plain _t;
var _t t = _t(11);

var& _t? t_ = &t;

var int ret = t_!.x;
t_!.x = 100;

escape ret + _id(t_!.x) + t.x;
]],
    run = 211,
}

Test { [[
class Tx with
    var& int? v;
do
    if v? then end;
end
var Tx t;
escape 1;
]],
    run = 1,
}

Test { [[
native _myalloc;
native do
    void* myalloc (void) {
        escape NULL;
    }
    void myfree (void* v) {
    }
end
native/nohold _myfree;

var& void? v;
    do v = &_myalloc();
finalize with
    _myfree(&&v!);
end

escape 1;
]],
    asr = true,
}

Test { [[
native _myalloc;
native do
    void* myalloc (void) {
        escape NULL;
    }
    void myfree (void* v) {
    }
end
native/nohold _myfree;

var& void? v;
    do v = &_myalloc();
finalize with
    if v? then
        _myfree(&&v!);
    end
end

escape 1;
]],
    run = 1,
}

Test { [[
native do
    ##define UNSAFE_POINTER_TO_REFERENCE(ptr) ptr
end
native/nohold _UNSAFE_POINTER_TO_REFERENCE;

native do
    int v2 = 10;
    int* V1 = NULL;
    int* V2 = &v2;
    int* fff (int i) {
        if (i == 1) {
            escape NULL;
        } else {
            escape V2;
        }
    }
end

var& int? v1;
native _fff;
        do v1 = &_fff(1);
    finalize with
        nothing;
    end

var& int? v2;
        do v2 = &_fff(2);
    finalize with
        nothing;
    end

var& int? v3;
native _V1, _V2;
        do v3 = &_UNSAFE_POINTER_TO_REFERENCE(_V1);
    finalize with
        nothing;
    end

var& int? v4;
        do v4 = &_UNSAFE_POINTER_TO_REFERENCE(_V2);
    finalize with
        nothing;
    end

escape (not v1?) + (not v3?) + v2? + v4? + (&&v2! ==_V2) + (&&v4! ==_V2) + v2!  
+ v4!;
]],
    run = 26,
}

Test { [[
data SDL_Color with
    var int v;
end
interface UI with
    var SDL_Color? bg_clr;
end
escape 1;
]],
    run = 1,
}

Test { [[
data SDL_Color with
    var int v;
end
var SDL_Color clr = SDL_Color(10);
var SDL_Color? bg_clr = clr;
escape bg_clr.v;
]],
    env = 'line 6 : invalid `.´ operation : cannot be an option type',
}
Test { [[
data SDL_Color with
    var int v;
end
var SDL_Color clr = SDL_Color(10);
var SDL_Color? bg_clr = clr;
escape bg_clr!.v;
]],
    run = 10,
}
Test { [[
data SDL_Color with
    var int v;
end
var SDL_Color? bg_clr = SDL_Color(10);
escape bg_clr!.v;
]],
    run = 10,
}

Test { [[
data SDL_Color with
    var int v;
end
class UI with
    var SDL_Color? bg_clr;
do
end
var UI ui with
    this.bg_clr = SDL_Color(10);
end;
escape ui.bg_clr!.v;
]],
    run = 10,
}

Test { [[
native do
    ##define fff(id) id
end
data SDL_Color with
    var int v;
end
class UI with
    var SDL_Color? bg_clr;
do
end
var UI ui with
    this.bg_clr = SDL_Color(10);
end;
native _fff;
escape _fff(ui.bg_clr!).v;
]],
    run = 10,
}

Test { [[
var int ret=0;
var& int? p = &ret;
p! = p!;
escape 1;
]],
    run = 1,
}

Test { [[
data OptionInt;
data Nil1 is OptionInt;
data Some1 is OptionInt with
    var int v;
end

data OptionPtr;
data Nil2 is OptionPtr;
data Some2 is OptionPtr with
    var int&& v;
end

var int ret = 0;    // 0

var int?  i;
var& int? p;
ret = ret + (not i?) + (not p?);  // 2

i = 3;
ret = ret + i!;      // 5

// first
p = &ret;
p! = p! + 2;          // 7
native _assert;
_assert(ret == 7);

// second
var int v = 10;
p! = v;              // 10
p! = p! + 1;          // 11

ret = ret + v;      // 21
escape ret;
]],
    wrn = true,
    run = 21,
}

Test { [[
var int? v1 = 10;
var int? v2 = v1;
escape v2!;
]],
    run = 10;
}

Test { [[
var int? v1 = 10;
var int? v2;
var int ret = v1? + v1!;
v1 = v2;
escape ret+v1?+1;
]],
    run = 12;
}

Test { [[
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v;
    do v = &_getV();
finalize with
    nothing;
end

class Tx with
    var& int? v;
do
    v! = 20;
end
do Tx with
    this.v = &v;
end;

escape v!;
]],
    env = 'line 21 : invalid operand to unary "&" : cannot be aliased',
}

Test { [[
native do
    int V = 10;
    int* getV (void) {
        escape &V;
    }
end

var& int? v;
    do v = &_getV();
finalize with
    nothing;
end

class Tx with
    var& int? v;
do
    v! = 20;
end
do Tx with
    this.v = &v!;
end;

escape v!;
]],
    run = 20,
}

--<<< OPTION TYPES

-- cannot compare ADTs
Test { DATA..[[
var Pair p1 = Pair(1,2);
var Pair p2 = Pair(1,2);
escape p1==p2;
]],
    wrn = true,
    env = 'line 53 : invalid operation for data',
    --run = 1,
}
Test { DATA..[[
pool[] List l1, l2;
l2 = new Nil();
escape l1==l2;
]],
    wrn = true,
    env = 'line 53 : invalid operands to binary "=="',
    --run = 1,
}

-- cannot mix recursive ADTs
Test { DATA..[[
pool[] List l1, l2;
l1 = new Cons(1, Nil());
l2 = new Cons(2, Nil());
((l1 as Cons).tail) = l2;
escape (((l1 as Cons).tail) as Cons).head;
]],
    wrn = true,
    --adt = 'line 54 : invalid attribution : value is not a reference',
    adt = 'line 54 : invalid attribution : mutation : cannot mix data sources',
}
Test { DATA..[[
pool[] List l1 = new Cons(1, Nil());
do
    pool[] List l2;
    l2 = new Cons(2, Nil());
    ((l1 as Cons).tail) = &&l2;
end
escape (((l1 as Cons).tail) as Cons).head;
]],
    wrn = true,
    adt = 'line 55 : invalid attribution : destination is not a reference',
    --adt = 'line 55 : cannot mix recursive data sources',
    --fin = 'line 54 : attribution to pointer with greater scope',
}
Test { DATA..[[
pool[] List l1;
l1 = new Cons(1, Nil());
pool[2] List l2 = new Cons(2, Nil());
((l1 as Cons).tail) = l2;
escape (((l1 as Cons).tail) as Cons).head;
]],
    wrn = true,
    --adt = 'line 54 : invalid attribution : value is not a reference',
    adt = 'line 54 : invalid attribution : mutation : cannot mix data sources',
}
Test { DATA..[[
pool[2] List l1;
pool[2] List l2;
l1 = new Cons(1, Nil());
l2 = new Cons(2, Nil());
((l1 as Cons).tail) = l2;
escape (((l1 as Cons).tail) as Cons).head;
]],
    wrn = true,
    --adt = 'line 55 : invalid attribution : value is not a reference',
    adt = 'line 55 : invalid attribution : mutation : cannot mix data sources',
}

Test { DATA..[[
var int ret = 0;                // 0

pool[5] List l;

// change head [2]
l = new Cons(1, Nil());
ret = ret + ((l as Cons).head);        // 2
native _assert;
_assert(ret == 1);

// add 2 [1, 2]
((l as Cons).tail) = new Cons(1, Nil());
ret = ret + ((l as Cons).head);        // 3
ret = ret + ((l as Cons).head) + (((l as Cons).tail) as Cons).head;
                                // 6
_assert(ret == 6);

// change tail [1, 2, 4]
(((l as Cons).tail) as Cons).tail = new Cons(4, Nil());
                                // 10

pool[] List l3 = new Cons(3, Nil());
(((l as Cons).tail) as Cons).tail = &&l3;
_assert((((l as Cons).tail) as Cons).head == 3);
_assert(((((l as Cons).tail) as Cons).tail as Cons).head == 4);
ret = ret + (((l as Cons).tail) as Cons).head + ((((l as Cons).tail) as Cons).tail as Cons).head;
                                // 17

// drop middle [1, 3, 4]
((l as Cons).tail) = (((l as Cons).tail) as Cons).tail;
ret = ret + (((l as Cons).tail) as Cons).head;
                                // 20

// fill the list [1, 3, 4, 5, 6] (7 fails)
((((l as Cons).tail) as Cons).tail as Cons).tail =
    new Cons(5, Cons(6, Cons(7, Nil())));

escape ret;
]],
    wrn = true,
    adt = 'line 72 : invalid attribution : destination is not a reference',
    --adt = 'line 72 : invalid attribution : new reference only to root',
    --adt = 'line 72 : cannot mix recursive data sources',
    run = -1,
}

Test { [[
interface IGUI_Component with
    var& _void? nat;
end

class EnterLeave with
    var& IGUI_Component gui;
do
    var _void&& g = &&(gui.nat!);
    if g then end;
end
escape 1;
]],
    run = 1,
}

Test { [[
class Tx with
    var int? x;
do
end

class U with
    var& Tx t;
do
end

var Tx t with
    this.x = 10;
end;

var U u with
    this.t = &t;
end;

escape u.t.x!;
]],
    run = 10,
}

Test { [[
class Tx with
    var int? x;
do
end

class U with
    var& Tx t;
    var int ret=0;
do
    this.ret = t.x!;
end

var Tx t with
    this.x = 10;
end;

var U u with
    this.t = &t;
end;

escape u.t.x! + u.ret;
]],
    run = 20,
}

Test { [[
class Tx with
    var& int? x;
do
end

class U with
    var& Tx t;
    var int ret=0;
do
    this.ret = t.x!;
end

var int z = 10;

var Tx t with
    this.x = &z;
end;

var U u with
    this.t = &t;
end;

escape u.t.x! + u.ret;
]],
    run = 20,
}

Test { [[
interface I with
    var int? x;
end

class Tx with
    interface I;
do
end

class U with
    var& Tx t;
do
end

var Tx t with
    this.x = 10;
end;

var U u with
    this.t = &t;
end;

escape u.t.x!;
]],
    run = 10,
}

Test { [[
interface I with
    var int? v;
end

class U with
    interface I;
do
end

var U u with
    this.v = 10;
end;
var I&& i = &&u;

escape i:v!;
]],
    run = 10,
}

Test { [[
class Tx with
    var int? x;
do
end

interface I with
    var& Tx t;
end

class U with
    interface I;
do
end

var Tx t with
    this.x = 10;
end;

var U u with
    this.t = &t;
end;
var I&& i = &&u;

escape ((*i).t).x!;
]],
    run = 10,
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;
var List&& lll = &&list;

escape 1;
]],
    wrn = true,
    run = 1,
    --env = 'line 15 : invalid operand to unary "&&"',
    --env = 'line 15 : data must be a pointer',
}
Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;
var List&& lll = list;

escape 1;
]],
    wrn = true,
    adt = 'line 11 : invalid attribution : mutation : cannot mix data sources',
    --adt = 'line 11 : invalid attribution : types mismatch (`List&&´ <= `List[]´)',
    --run = 1,
    --env = 'line 15 : invalid operand to unary "&&"',
    --env = 'line 15 : data must be a pointer',
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list

= new Cons(10, Nil());
var List&& l = list;

watching l do
    await 1s;
end

escape 0;
]],
    env = 'line 15 : not a struct',
    --env = 'line 15 : invalid operand to unary "&&"',
    --env = 'line 15 : data must be a pointer',
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());

pool&[] List lll = &list;

escape (lll as Cons).head;
]],
    run = 10,
}
Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());

pool[] List&& lll = &&list;

escape (lll as Cons).head;
]],
    run = 10,
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());
pool[] List&& l = &&list;

((l as Cons).tail) = new Cons(9, Nil());
l = ((l as Cons).tail);

((l as Cons).tail) = new Cons(8, Nil());
l = ((l as Cons).tail);

escape (*l as Cons) +
        (list as Cons).head +
        ((list as Cons).tail as Cons).head +
        ((((list as Cons).tail as Cons).tail) as Cons).head;
]],
    adt = 'line 16 : invalid attribution : mutation : destination cannot be a pointer',
}

-- mutation in the root of &&
--  also, the other way around is unsafe
--   which is a problem
Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());

pool[] List&& l1 = &&list;
pool&[] List  l2 = &list;

list = new Nil();

escape (*l1 as Cons)+(l2 as Cons)+(list as Cons)+1;
]],
    --run = 1,
    fin = 'line 19 : unsafe access to pointer "l1" across `assignment´ (tests.lua : 17)',
}

-- mutation in the root of &&
--  also, the other way around is unsafe
--   which is a problem
Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());
pool[] List&& lll = &&list;

var int ret = 0;

watching *lll do
    *lll = (lll as Cons).tail;
    ret = (*lll as Cons) +
            (list as Cons).head +
            (list as Cons).tail is Nil;
end

escape ret;
]],
    adt = 'line 18 : invalid attribution : mutation : cannot mutate root of a reference',
    --run = '17] runtime error: invalid tag',
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Cons(20, Nil()));
pool[] List&& lll = &&list;

var int ret = 0;
watching *lll do
    (lll as Cons).tail = (((lll as Cons).tail) as Cons).tail;
    ret = (*lll as Cons) +
            (list as Cons).head +
            ((list as Cons).tail is Nil);
end
escape ret;
]],
    _ana = {acc=true},
    run = 12,
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());
vector[] List&& lll = &&list; // TODO fat pointer

*lll = (lll as Cons).tail;

escape (*lll as Cons) + (list as Cons) + 1;
]],
    adt = 'line 15 : invalid attribution : mutation : cannot mutate from pointers',
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());
pool[] List&& lll = &&list;

*lll = (lll as Cons).tail;

escape (*lll as Cons) +
        (list as Cons).head +
        (list as Cons).tail is Nil;
]],
    --run = 10,
    adt = 'line 15 : invalid attribution : mutation : cannot mutate root of a reference',
}
Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());
pool[] List&& lll = &&list;

(lll as Cons).tail = new Cons(9, Nil());
*lll = (lll as Cons).tail;

(lll as Cons).tail = new Cons(8, Nil());
*lll = (lll as Cons).tail;

escape (*lll as Cons) +
        (list as Cons).head +
        ((list as Cons).tail is Nil);
]],
    adt = 'line 16 : invalid attribution : mutation : cannot mutate root of a reference',
}
Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());
pool[] List&& l = &&list;

var int ret = 0;

watching *l do
    ((l as Cons).tail) = new Cons(9, Nil());
    l = &&((l as Cons).tail);

    ((l as Cons).tail) = new Cons(8, Nil());
    l = &&((l as Cons).tail);

    ret = (*l as Cons) +
            (list as Cons).head +
            ((list as Cons).tail as Cons).head +
            ((((list as Cons).tail as Cons).tail) as Cons).head;
end
escape ret;
]],
    _ana = {acc=true},
    run = 28,
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list;

list = new Cons(10, Nil());
pool[] List&& l = &&list;

watching *l do
    ((l as Cons).tail) = new Cons(9, Nil());
    l = &&((l as Cons).tail);

    await 1s;

    ((l as Cons).tail) = new Cons(8, Nil());
    l = &&((l as Cons).tail);

    escape ((l as Cons).head) +
            (list as Cons).head +
            ((list as Cons).tail as Cons).head +
            ((((list as Cons).tail as Cons).tail) as Cons).head;
end

escape 0;
]],
    _ana = {acc=true},
    run = { ['~>1s'] = 35 },
}

-- fails if inner is killed before outer
Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[10] List list;

list = new Cons(10, Nil());
pool[] List&& lll = &&list;

watching *lll do
    (lll as Cons).tail = new Cons(9, Nil());
    lll = &&(lll as Cons).tail;

    par do
        watching *lll do
            await 1s;

            (lll as Cons).tail = new Cons(8, Nil());
            lll = &&(lll as Cons).tail;

            escape (lll as Cons).head +
                    (list as Cons).head +
                    ((list as Cons).tail as Cons).head +
                    (((list as Cons).tail as Cons).tail as Cons).head;
        end
        escape 1;
    with
        list = new Nil();
        await FOREVER;
    end
end
escape -1;
]],
    _ana = {acc=true},
    run = -1,
}

Test { [[
data List;
data Nil is List;
data Cons is List with
    var int  head;
    var List tail;
end

pool[] List list = new Cons(10, Nil());
pool[] List&& lll = &&list;

watching *lll do
    (lll as Cons).tail = new Cons(9, Nil());
    lll = &&(lll as Cons).tail;

    par do
        await 1s;

        (lll as Cons).tail = new Cons(8, Nil());
        lll = &&(lll as Cons).tail;

        escape (lll as Cons).head +
                (list as Cons).head +
                ((list as Cons).tail as Cons).head +
                (((list as Cons).tail as Cons).tail as Cons).head;
    with
        list = new Nil();
        await FOREVER;
    end
end
escape -1;
]],
    _ana = {acc=true},
    run = -1,
}

-->>> DATA + VECTORS + REFERENCES
Test { [[
data Test with
    var u8 v;
end
var Test a = Test(1);
var Test b = Test(2);
var Test c = Test(3);
vector[3] Test vs = [ a, b, c ];
escape vs[0].v + vs[1].v + vs[2].v;
]],
    run = 6,
}

Test { [[
data Test with
    var& u8 v;
end

var u8 v = 7;
var Test t=Test(&v);
v = 10;
escape t.v;
]],
    run = 10,
}

Test { [[
data Test with
    var& u8 v;
end
var u8 v = 7;
var Test t = Test(&v);
v = 10;
escape t.v;
]],
    run = 10,
}

Test { [[
data Test with
    var& u8 v;
end

var u8 v = 7;
vector[3] Test vs;
var Test t = Test(&v);
vs = [] .. vs .. [t];

v = 10;
t.v = 88;
vs[0].v = 36;
escape v;
]],
    run = 36,
}

Test { [[
  vector[3] u8 bytes;

bytes = [] .. bytes .. [5];

escape bytes[0];
]],
    run = 5,
}
Test { [[
data Frame with
  vector[3] u8 bytes;
end

var Frame f1;
f1.bytes = [] .. f1.bytes .. [5];

escape f1.bytes[0];
]],
    env = 'line 2 : `data´ fields do not support vectors yet',
}
Test { [[
data Frame;
    data Xx is Frame;
    data Yy is Frame with
        vector[3] u8 bytes;
    end

escape 1;
]],
    wrn = true,
    env = 'line 5 : `data´ fields do not support vectors yet',
}
Test { [[
native _u8;
data Frame with
  var _u8[3] bytes;
end

vector[10] Frame frames;
var Frame f1;

f1.bytes[0] = 5;

frames = [] .. frames .. [f1];

escape frames[0].bytes[0];
]],
    ref = 'line 8 : invalid access to uninitialized variable "f1" (declared at tests.lua:6)'
}

Test { [[
data Tx with
    var& int i;
end
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { [[
data Tx with
    vector&[] byte str;
end
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { [[
pre native do
    typedef byte* char_ptr;
end
native/nohold _strlen;
native/plain _char_ptr;
data Dx with
    var _char_ptr str;
end
vector[] byte s = [].. "oi";
native _char;
var Dx d = Dx(&&s as _char&& as _char_ptr);
escape _strlen(d.str as _char&&);
]],
    run = 2,
}
Test { [[
data Dx with
    vector&[] byte str;
end
vector[] byte s = [].. "oi";
var Dx d = Dx(&s);
escape $d.str;
]],
    run = 2,
}

--<<< DATA + VECTORS

-- DATA / RECURSE / TRAVERSE

-- crashes with org->ret
Test { [[
class Tx with
do
    await FOREVER;
end

event Tx&& e;

input void OS_START;

par do
    do
        par/or do
            await OS_START;
            pool[1] Tx ts;
            var Tx&&? ptr = spawn Tx in ts;
            emit e => ptr!;
        with
            var Tx&& t = await e;
        end
    end
    do
native _int;
        var _int[100] iss = [];
        loop i in [0 |> 100[ do
            iss[i] = i;
        end
    end
    await 1s;
    escape 1;
with
    var Tx&& t = await e;
    var int ret = await *t;     // crash!
    escape ret;
end
]],
    env = 'line 6 : invalid event type',
    --env = 'line 16 : wrong argument : cannot pass pointers',
    --run = { ['~>1s']=1 },
}

Test { [[
data Widget;
    data Nil is Widget;
    data Row is Widget with
        var Widget w1;
    end

pool[] Widget widgets;
traverse widget in &&widgets do
    watching *widget do
        var int v1 = traverse &&(*widget as Row).w1;
    end
end

escape 1;
]],
    _ana = {acc=true},
    wrn = true,
    run = 1,
}

-- leaks memory because of lost "free" in IN__STK
Test { [[
data Tx;
    data Nil is Tx;
    data Nxt is Tx with
        var int v;
        var Tx   nxt;
    end

pool[] Tx ts = new Nxt(10, Nxt(9, Nil()));

par/or do
    await ts;           // 2. but continuation is aborted
with
    ts = new Nil();   // 1. free is on continuation
end

escape 1;
]],
    _ana = { acc=true },
    run = 1,
}

Test { [[
data Tx;
    data Nil is Tx;
    data Nxt is Tx with
        var int v;
        var Tx   nxt;
    end

pool[] Tx ts;

ts = new Nxt(10, Nxt(9, Nil()));

var int ret = 10;

par/or do
    watching ts do
        await FOREVER;
    end
    ret = ret * 2;
with
    watching (ts as Nxt).nxt do
        await FOREVER;
    end
    ret = 0;
with
    watching ((ts as Nxt).nxt as Nxt).nxt do
        await FOREVER;
    end
    ret = ret - 1;  // awakes first from Nil
    await FOREVER;
with
    ts = new Nil();
    ret = 0;
end

escape ret;
]],
    _ana = { acc=true },
    run = 18,
}

Test { [[
class Body with
    pool&[]  Body bodies;
    var&   int    sum;
    event int     ok;
do
    do finalize with end;

    var Body&&? nested =
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    if nested? then
        watching *nested! do
            await nested!:ok;
        end
    end
    await 1s;
    sum = sum + 1;
    emit this.ok => 1;
end


pool[100] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;
await b;

escape sum;
]],
    wrn = 'line 9 : unbounded recursive spawn',
    run = { ['~>200s'] = 101 },
}
Test { [[
class Body with
    pool&[]  Body bodies;
    var&   int    sum;
    event int     ok;
do
    do finalize with end;

    var Body&&? nested =
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    if nested? then
        watching *nested! do
            await nested!:ok;
        end
    end
    await 1s;
    sum = sum + 1;
    emit this.ok => 1;
end


pool[] Body bodies;
var  int     sum = 0;

var Body b with
    this.bodies = &bodies;
    this.sum    = &sum;
end;
await b;

escape sum;
]],
    wrn = 'line 9 : unbounded recursive spawn',
    run = { ['~>200s'] = 101 },
}
Test { [[
class Body with
    pool&[3]  Body bodies;
    var&   int    sum;
    event int     ok;
do
    do finalize with end;

    var Body&&? nested =
        spawn Body in bodies with
            this.bodies = &bodies;
            this.sum    = &sum;
        end;
    if nested? then
        watching *nested! do
            await nested!:ok;
        end
    end
    await 1s;
    sum = sum + 1;
    emit this.ok => 1;
end


pool[3] Body bodies;
var  int     sum = 0;

var Body&&? b = spawn Body in bodies with
    this.bodies = &bodies;
    this.sum    =& sum;
end;
await *b!;

escape sum;
]],
    _ana = {acc=true},
    run = { ['~>10s'] = 3 },
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var Tree left;
    end
var Tree&& n;
if false then
    await *n;
end
escape 1;
]],
    wrn = true,
    ref = 'line 10 : invalid access to uninitialized variable "n" (declared at tests.lua:8)',
    --run = 1,
}
Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var Tree left;
    end
class Body with
    var Tree&& n;
do
    if false then
        await *(this.n);
    end
end
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

class Body with
    pool&[]  Body bodies;
    var   Tree&&   n;
    var&   int    sum;
    event int     ok;
do
    watching *n do
        var int i = this.sum;
        if i then end;
        if (*n is Node) then
            var Body&&? left =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n as Node).left;
                    this.sum    = &sum;
                end;
            if left? then
                watching *left! do
                    await left!:ok;
                end
            end

            this.sum = this.sum + 1;

            var Body&&? right =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n as Node).right;
                    this.sum    = &sum;
                end;
            if right? then
                watching *right! do
                    await right!:ok;
                end
            end
        end
    end
    await 1s;
    emit this.ok => 1;
end

var int sum = 0;

pool[7] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&tree;
    this.sum    = &sum;
end;

escape sum;
]],
    _ana = {acc=true},
    wrn = 'line 26 : unbounded recursive spawn',
    run = { ['~>10s'] = 3 },
}
Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

class Body with
    pool&[]  Body bodies;
    var   Tree&&   n;
    var&   int    sum;
    event int     ok;
do
    watching *n do
        var int i = this.sum;
        if (*n is Node) then
            var Body&&? left =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n as Node).left;
                    this.sum    = &sum;
                end;
            if left? then
                watching *left! do
                    await left!:ok;
                end
            end

            this.sum = this.sum + i + (*n as Node).v;

            var Body&&? right =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n as Node).right;
                    this.sum    = &sum;
                end;
            if right? then
                watching *right! do
                    await right!:ok;
                end
            end
        end
    end
    await 1s;
    emit this.ok => 1;
end

var int sum = 0;

pool[7] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&tree;
    this.sum    = &sum;
end;

escape sum;
]],
    wrn = 'line 26 : unbounded recursive spawn',
    run = { ['~>10s'] = 9 },
}
Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

class Body with
    pool&[7]  Body bodies;
    var   Tree&&    n;
    var&   int     sum;
    event int      ok;
do
    watching *n do
        var int i = this.sum;
        if (*n is Node) then
            var Body&&? left =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n as Node).left;
                    this.sum    = &sum;
                end;
            if left? then
                watching *left! do
                    await left!:ok;
                end
            end

            this.sum = this.sum + i + (*n as Node).v;

            var Body&&? right =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n as Node).right;
                    this.sum    = &sum;
                end;
            if right? then
                watching *right! do
                    await right!:ok;
                end
            end

            //do/spawn Body in this.bodies with
                //this.n = (*n as Node).left;
            //end;
        end
    end
    await 1s;
    emit this.ok => 1;
end

var int sum = 0;

pool[7] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&tree;
    this.sum    = &sum;
end;

escape sum;
]],
    run = { ['~>10s'] = 9 },
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list
    = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

class Body with
    pool&[]  Body bodies;
    var   List&&   n;
do
    await 1s;
    if (*n is Nil) then
    end
    watching *n do
        if (*n is Cons) then
            spawn Body in this.bodies with
                this.bodies = &bodies;
                this.n      = &&(*n is Cons).tail;
            end;
        end
    end
end

pool[3] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&list;
end;

escape 1;
]],
    fin = 'line 20 : unsafe access to pointer "n" across `await´ (tests.lua : 19)',
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list
    = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

class Body with
    pool&[]  Body bodies;
    var   List&&   n;
do
    if (*n is Nil) then
    end
    watching *n do
        if (*n is Cons) then
            spawn Body in this.bodies with
                this.bodies = &bodies;
                this.n      = &&(*n is Cons).tail;
            end;
        end
    end
end

pool[3] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&list;
end;

escape 1;
]],
    wrn = true,
    run = 1,
    --fin = 'line 19 : unsafe access to pointer "n" across `class´ (tests.lua : 15)',
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    if (*n is Cons) then
        sum = sum + (*n is Cons).head;
        traverse &&(*n is Cons).tail;
    end
end

escape sum;
]],
    run = 10,
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    if (*n is Cons) then
        sum = sum + (*n is Cons).head;
        await 1s;
        traverse &&(*n is Cons).tail;
    end
end

escape sum;
]],
    fin = 'line 22 : unsafe access to pointer "n" across `await´ (tests.lua : 21)',
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    await 1s;
    if (*n is Cons) then
        sum = sum + (*n is Cons).head;
        traverse &&(*n is Cons).tail;
    end
end

escape sum;
]],
    fin = 'line 20 : unsafe access to pointer "n" across `await´ (tests.lua : 19)',
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    if (*n is Cons) then
        sum = sum + (*n is Cons).head;
        watching *n do
            await 1s;
            traverse &&(*n is Cons).tail;
        end
    end
end

escape sum;
]],
    run = { ['~>10s'] = 10 },
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    //watching *n do
        //await 1s;
        if (*n is Cons) then
            sum = sum + (*n is Cons).head;
            traverse &&(*n is Cons).tail;
        end
    //end
end

escape sum;
]],
    run = { ['~>10s'] = 10 },
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    //watching *n do
        await 1s;
        if (*n is Cons) then
            sum = sum + (*n is Cons).head;
            traverse &&(*n is Cons).tail;
        end
    //end
end

escape sum;
]],
    fin = 'line 21 : unsafe access to pointer "n" across `await´ (tests.lua : 20)',
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list
    = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    if (*n is Nil) then
        sum = sum * 2;
    end
    watching *n do
        if (*n is Cons) then
            sum = sum + (*n is Cons).head;
            traverse &&(*n is Cons).tail;
        end
    end
end

escape sum;
]],
    wrn = 'line 42 : unbounded recursive spawn',
    _ana = { acc=true },
    run = 12,
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree =
    new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int  v = 0;
var int&& ptr = &&v;

traverse t in &&tree do
    *ptr = *ptr + 1;
    if (*t is Node) then
        traverse &&(*t as Node).left;
        traverse &&(*t as Node).right;
    end
end

escape v;
]],
    fin = 'line 23 : unsafe access to pointer "t" across `spawn´ (tests.lua : 22)',
    --run = 7,
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree =
    new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int  v = 0;
var int&& ptr = &&v;

traverse t in &&tree do
    *ptr = *ptr + 1;
    if (*t is Node) then
        watching *t do
            traverse &&(*t as Node).left;
            traverse &&(*t as Node).right;
        end
    end
end

escape v;
]],
    --fin = 'line 20 : unsafe access to pointer "ptr" across `class´',
    run = 7,
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

native do
    int V = 0;
end

/*
traverse n in &&list do
    _V = _V + 1;
    if (*n is Cons) then
        _V = _V + (*n is Cons).head;
        traverse &&(*n is Cons).tail;
    end
end
*/

class Body with
    pool&[3]  Body bodies;
    var   List&&    n;
do
    if (*n is Nil) then
native _V;
        _V = _V * 2;
    end
    watching *n do
        _V = _V + 1;
        if (*n is Cons) then
            _V = _V + (*n is Cons).head;

            var Body&&? tail =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n is Cons).tail;
                end;
            if tail? then
                await *tail!;
            end
        end
    end
end

pool[3] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&list;
end;

escape _V;
]],
    --fin = 'line 33 : unsafe access to pointer "n" across `class´',
    _ana = {acc=true},
    run = 18,
}

Test { [[
data List;
    data Nil_;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[4] List list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

native do
    int V = 0;
end

/*
traverse n in &&list do
    _V = _V + 1;
    if (*n is Cons) then
        _V = _V + (*n is Cons).head;
        traverse &&(*n is Cons).tail;
    end
end
*/

class Body with
    pool&[4]  Body bodies;
    var   List&&    n;
do
    watching *n do
        if (*n is Nil) then
native _V;
            _V = _V * 2;
        else/if (*n is Cons) then
            _V = _V + 1;
            _V = _V + (*n is Cons).head;

            var Body&&? tail =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n is Cons).tail;
                end;
            if tail? then
                await *tail!;
            end
        end
    end
end

pool[4] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&list;
end;

escape _V;
]],
    _ana = { acc=true },
    run = 18,
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

native do
    int V = 0;
end

traverse n in &&list do
native _V;
    _V = _V + 1;
    if (*n is Cons) then
        _V = _V + (*n is Cons).head;
        traverse &&(*n is Cons).tail;
    end
end

/*
class Body with
    pool&[]  Body bodies;
    var   List&&   n;
do
    watching *n do
        _V = _V + 1;
        if (*n is Cons) then
            _V = _V + (*n is Cons).head;

            var Body&&? tail =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n is Cons).tail;
                end;
            if tail? then
                await *tail!;
            end
        end
    end
end

pool[3] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&list;
end;
*/

escape _V;
]],
    _ana = { acc=true },
    wrn = 'line 23 : unbounded recursive spawn',
    run = 10,
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

native do
    int V = 0;
end

traverse n in &&list do
native _V;
    _V = _V + 1;
    if (*n is Cons) then
        _V = _V + (*n is Cons).head;
        traverse &&(*n is Cons).tail;
    end
end

/*
class Body with
    pool&[]  Body bodies;
    var   List&&   n;
do
    watching *n do
        _V = _V + 1;
        if (*n is Cons) then
            _V = _V + (*n is Cons).head;

            var Body&&? tail =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n is Cons).tail;
                end;
            if tail? then
                await *tail!;
            end
        end
    end
end

pool[3] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&list;
end;
*/

escape _V;
]],
    _ana = { acc=true },
    run = 10,
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    watching *n do
        await 1s;
        if (*n is Cons) then
            sum = sum + (*n is Cons).head;
            traverse &&(*n is Cons).tail;
        end
    end
end

escape sum;
]],
    run = { ['~>10s'] = 10 },
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    //watching *n do
        await 1s;
        if (*n is Cons) then
            sum = sum + (*n is Cons).head;
            traverse &&(*n is Cons).tail;
        end
    //end
end

escape sum;
]],
    --run = { ['~>10s'] = 10 },
    fin = 'line 21 : unsafe access to pointer "n" across `await´ (tests.lua : 20)',
}

Test { [[
native do
##ifdef CEU_ORGS_NEWS_MALLOC
##error "malloc found"
##endif
end

data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;
traverse n in &&list do
    sum = sum + 1;
    watching *n do
        await 1s;
        if (*n is Cons) then
            sum = sum + (*n is Cons).head;
            traverse &&(*n is Cons).tail;
            sum = sum + (*n is Cons).head;
        end
    end
end

escape sum;
]],
    run = { ['~>10s'] = 16 },
}

Test { [[
native do
##ifdef CEU_ORGS_NEWS_MALLOC
##error "malloc found"
##endif
end

data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

class Tx with
do
    pool[3] List list;
    list = new Cons(1,
                Cons(2,
                    Cons(3, Nil())));

    var int sum = 0;
    traverse n in &&list do
        sum = sum + 1;
        watching *n do
            await 1s;
            if (*n is Cons) then
                sum = sum + (*n is Cons).head;
                traverse &&(*n is Cons).tail;
                sum = sum + (*n is Cons).head;
            end
        end
    end
    escape sum;
end

var int sum = do Tx;

escape sum;
]],
    run = { ['~>10s'] = 16 },
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 0;

traverse n in &&tree do
    sum = sum + 1;
    watching *n do
        if (*n is Node) then
            traverse &&(*n as Node).left;
            sum = sum + (*n as Node).v;
            traverse &&(*n as Node).right;
        end
    end
end

escape sum;
]],
    wrn = 'line 22/24 : unbounded recursive spawn',
    run = 13,
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 0;

traverse n in &&tree do
    sum = sum + 1;
    watching *n do
        if (*n is Node) then
            traverse &&(*n as Node).left;
            sum = sum + (*n as Node).v;
            traverse &&(*n as Node).right;
        end
    end
end

escape sum;
]],
    run = 13,
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 0;

traverse n in &&tree do
    sum = sum + 1;
    watching *n do
        if (*n is Node) then
            await 1s;
            traverse &&(*n as Node).left;
            sum = sum + (*n as Node).v;
            traverse &&(*n as Node).right;
        end
    end
end

escape sum;
]],
    run = { ['~>10s'] = 13 },
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 1;

traverse n in &&tree do
    watching *n do
        if (*n is Node) then
            traverse &&(*n as Node).left;
            sum = sum * (*n as Node).v + (*n as Node).v;
            traverse &&(*n as Node).right;
        end
    end
end

escape sum;
]],
    wrn = 'line 22/24 : unbounded recursive spawn',
    run = { ['~>10s'] = 18 },
}
Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 1;

traverse n in &&tree do
    watching *n do
        if (*n is Node) then
            traverse &&(*n as Node).left;
            sum = sum * (*n as Node).v + (*n as Node).v;
            traverse &&(*n as Node).right;
        end
    end
end

escape sum;
]],
    run = { ['~>10s'] = 18 },
}

Test { [[
data Tx;
    data Nil is Tx;
    data Nxt is Tx with
        var int v;
        var Tx   nxt;
    end

pool[] Tx ts;

var void&& p1 = this as void&&;

traverse t in &&ts do
native _assert;
    _assert(p1 == (this as void&&));
    if (*t is Nxt) then
        traverse &&(*t as Nxt).nxt;
    end
end

escape 1;
]],
    --fin = 'line 15 : unsafe access to pointer "p1" across `class´ (tests.lua : 14)',
    gcc = '12:5: error: cannot convert to a pointer type',
    wrn = true,
    run = 1,
}
Test { [[
data Tx;
    data Nil is Tx;
    data Nxt is Tx with
        var int v;
        var Tx   nxt;
    end

pool[] Tx ts;

var void&& p1 = &&this as void&&;

traverse t in &&ts do
native _assert;
    _assert(p1 == (&&this as void&&));
    if (*t is Nxt) then
        traverse &&(*t as Nxt).nxt;
    end
end

escape 1;
]],
    --fin = 'line 15 : unsafe access to pointer "p1" across `class´ (tests.lua : 14)',
    wrn = true,
    run = 1,
}
Test { [[
data Tx;
    data Nil is Tx;
    data Nxt is Tx with
        var int v;
        var Tx   nxt;
    end

pool[] Tx ts;

native do
    ##define PTR2REF(x) &x
end
var& void? p1;
    do p1 = &_PTR2REF(this);
finalize with
    nothing;
end

traverse t in &&ts do
native _assert;
    _assert(&&p1! == (&&this as void&&));
    if (*t is Nxt) then
        traverse &&(*t as Nxt).nxt;
    end
end

escape 1;
]],
    wrn = 'line 17 : unbounded recursive spawn',
    run = 1,
}
Test { [[
data Tx;
    data Nil is Tx;
    data Nxt is Tx with
        var int v;
        var Tx   nxt;
    end

pool[1] Tx ts;

native do
    ##define PTR2REF(x) &x
end
var& void? p1;
    do p1 = &_PTR2REF(this);
finalize with
    nothing;
end

traverse t in &&ts do
native _assert;
    _assert(&&p1! == (&&this as void&&));
    if (*t is Nxt) then
        traverse &&(*t as Nxt).nxt;
    end
end

escape 1;
]],
    run = 1,
}

Test { [[
data Tx;
    data Nil is Tx;
    data Nxt is Tx with
        var int v;
        var Tx   nxt;
    end

pool[] Tx ts;

native do
    ##define PTR2REF(x) &x
end
var& void? p1;
    do p1 = &_PTR2REF(this);
finalize with
    nothing;
end

var int v2 = 2;
var int v3 = 3;

class X with
    var int v1, v2, v3;
do end

traverse t in &&ts do
native _assert;
    _assert(&&p1! == (&&this as void&&));
    var int v1 = 1;
    var int v3 = 0;
    var X x with
        this.v1 = v1;
        this.v2 = v2;
        this.v3 = outer.v3;
    end;
    _assert(x.v1 + x.v2 + x.v3 == 6);
    if (*t is Nxt) then
        traverse &&(*t as Nxt).nxt;
    end
end

escape 1;
]],
    wrn = 'line 17 : unbounded recursive spawn',
    run = 1,
}
Test { [[
data Tx;
    data Nil is Tx;
    data Nxt is Tx with
        var int v;
        var Tx   nxt;
    end

pool[1] Tx ts;

native do
    ##define PTR2REF(x) &x
end
var& void? p1;
    do p1 = &_PTR2REF(this);
finalize with
    nothing;
end

var int v2 = 2;
var int v3 = 3;

class X with
    var int v1, v2, v3;
do end

traverse t in &&ts do
native _assert;
    _assert(&&p1! == (&&this as void&&));
    var int v1 = 1;
    var int v3 = 0;
    var X x with
        this.v1 = v1;
        this.v2 = v2;
        this.v3 = outer.v3;
    end;
    _assert(x.v1 + x.v2 + x.v3 == 6);
    if (*t is Nxt) then
        traverse &&(*t as Nxt).nxt;
    end
end

escape 1;
]],
    run = 1,
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 1;

traverse n in &&tree do
    watching *n do
        await 1s;
        if (*n is Node) then
            traverse &&(*n as Node).left;
            sum = sum * (*n as Node).v + (*n as Node).v;
            traverse &&(*n as Node).right;
        end
    end
end

escape sum;
]],
    run = { ['~>10s'] = 18 },
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 1;

traverse n in &&tree do
    await 1s;
    watching *n do
        if (*n is Node) then
            traverse &&(*n as Node).left;
            sum = sum * (*n as Node).v + (*n as Node).v;
            traverse &&(*n as Node).right;
        end
    end
end

escape sum;
]],
    fin = 'line 19 : unsafe access to pointer "n" across `await´ (tests.lua : 18)',
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 1;

do
    traverse n in &&tree do
        watching *n do
            await 1s;
            if (*n is Node) then
                traverse &&(*n as Node).left;
                sum = sum * (*n as Node).v + (*n as Node).v;
                traverse &&(*n as Node).right;
            end
        end
    end
end

escape sum;
]],
    run = { ['~>10s'] = 18 },
}

Test { [[
data Tree;
    data Nil is Tree;
    data Node is Tree with
        var int   v;
        var Tree  left;
        var Tree  right;
    end

pool[3] Tree tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 1;

par/and do
    traverse n in &&tree do
        watching *n do
            if (*n is Node) then
                await 1s;
                traverse &&(*n as Node).left;
                sum = sum * (*n as Node).v + (*n as Node).v;
                traverse &&(*n as Node).right;
                await 1s;
            end
        end
    end
    sum = sum - 1;
with
    await 1s;
native _ceu_out_assert_msg;
    _ceu_out_assert_msg(sum == 1, "1");
    await 1s;
    _ceu_out_assert_msg(sum == 4, "2");
    await 1s;
    _ceu_out_assert_msg(sum == 5, "3");
    tree = new Nil();
    _ceu_out_assert_msg(sum == 4, "4");
end

escape sum;
]],
    _ana = {acc=true},
    run = { ['~>20s'] = 4 },
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    if (*n is Cons) then
        sum = sum + (*n is Cons).head;
        loop i in [0 |> 1[ do
            traverse &&(*n is Cons).tail;
        end
    end
end

escape sum;
]],
    fin = 'line 22 : unsafe access to pointer "n" across `loop´ (tests.lua : 21)',
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    if (*n is Cons) then
        sum = sum + (*n is Cons).head;
        //loop i in [0 |> 1[ do
            traverse &&(*n is Cons).tail;
        //end
    end
end

escape sum;
]],
    wrn = 'line 24 : unbounded recursive spawn',
    run = 10,
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list
    = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

traverse n in &&list do
    sum = sum + 1;
    if (*n is Cons) then
        sum = sum + (*n is Cons).head;
        //loop i in [0 |> 1[ do
            traverse &&(*n is Cons).tail;
        //end
    end
end

escape sum;
]],
    run = 10,
}

Test { [[
loop do
    traverse null;
end
escape 1;
]],
    adj = 'line 2 : missing enclosing `traverse´ block',
}

Test { [[
traverse t in ts do
    loop do
        traverse/1 null;
    end
end
escape 1;
]],
    adj = 'line 3 : missing enclosing `traverse´ block',
}

Test { [[
data Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

pool[] Widget widgets;
widgets = new Seq(
            Empty(),
            Empty());

var int ret = 0;

traverse widget in &&widgets with
    var int param = 1;
do
    ret = ret + param;

    watching *widget do
        if (*widget is Empty) then
            nothing;

        else/if (*widget is Seq) then
            traverse &&(*widget as Seq).w1 with
                this.param = param + 1;
            end;
            traverse &&(*widget as Seq).w2 with
                this.param = param + 1;
            end;

        else
native _ceu_out_assert_msg;
            _ceu_out_assert_msg(0, "not implemented");
        end
    end
end

escape ret;
]],
    adt = 'line 23 : ineffective use of data "Empty" due to enclosing `watching´',
}
Test { [[
data Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

pool[] Widget widgets;
widgets = new Seq(
            Empty(),
            Empty());

var int ret = 0;

traverse widget in &&widgets with
    var int param = 1;
do
    ret = ret + param;

    watching *widget do
        if (*widget is Seq) then
            traverse &&(*widget as Seq).w1 with
                this.param = param + 1;
            end;
            traverse &&(*widget as Seq).w2 with
                this.param = param + 1;
            end;

        else
native _ceu_out_assert_msg;
            _ceu_out_assert_msg(0, "not implemented");
        end
    end
end

escape ret;
]],
    _ana = { acc=true },
    wrn = 'line 27/30 : unbounded recursive spawn',
    run = 5,
}
Test { [[
data Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

pool[10] Widget widgets;
widgets = new Seq(
            Empty(),
            Empty());

var int ret = 0;

traverse widget in &&widgets with
    var int param = 1;
do
    ret = ret + param;

    watching *widget do
        if (*widget is Seq) then
            traverse &&(*widget as Seq).w1 with
                this.param = param + 1;
            end;
            traverse &&(*widget as Seq).w2 with
                this.param = param + 1;
            end;

        else
native _ceu_out_assert_msg;
            _ceu_out_assert_msg(0, "not implemented");
        end
    end
end

escape ret;
]],
    _ana = { acc=true },
    wrn = 'line 27/30 : unbounded recursive spawn',
    run = 5,
}

Test { [[
data Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

pool[] Widget widgets
    = new Seq(
            Empty(),
            Empty());

var int ret = 0;

traverse widget in &&widgets with
    var int param = 1;
do
    ret = ret + param;

    watching *widget do
        if (*widget is Seq) then
            traverse &&(*widget as Seq).w1 with
                this.param = param + 1;
            end;
            traverse &&(*widget as Seq).w2 with
                this.param = param + 1;
            end;

        else
native _ceu_out_assert_msg;
            _ceu_out_assert_msg(0, "not implemented");
        end
    end
end

escape ret;
]],
    _ana = { acc=true },
    wrn = 'line 27/30 : unbounded recursive spawn',
    run = 5,
}
Test { [[
data Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

pool[10] Widget widgets = new Seq(
            Empty(),
            Empty());

var int ret = 0;

traverse widget in &&widgets with
    var int param = 1;
do
    ret = ret + param;

    watching *widget do
        if (*widget is Seq) then
            traverse &&(*widget as Seq).w1 with
                this.param = param + 1;
            end;
            traverse &&(*widget as Seq).w2 with
                this.param = param + 1;
            end;

        else
native _ceu_out_assert_msg;
            _ceu_out_assert_msg(0, "not implemented");
        end
    end
end

escape ret;
]],
    _ana = { acc=true },
    run = 5,
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[] List l = new Cons(1,
            Cons(2,
                Cons(3,
                    Cons(4,
                        Cons(5,
                            Nil())))));

var int ret = 0;

par/or do
    await (((l as Cons).tail) as Cons).tail;
    ret = 100;
with
    (((l as Cons).tail) as Cons).tail = new Nil();
    ret = 10;
end

escape ret;
]],
    _ana = {acc=true},
    run = 100,
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[] List l;
l = new Cons(1,
            Cons(2,
                Cons(3,
                    Cons(4,
                        Cons(5,
                            Nil())))));

var int ret = 0;

par/or do
    await (((l as Cons).tail) as Cons).tail;
    ret = ret + ((((l as Cons).tail) as Cons).tail as Cons).head;    // 0+4
native _ceu_out_assert_msg;
    _ceu_out_assert_msg(ret == 4, "1");
    (((l as Cons).tail) as Cons).tail = ((((l as Cons).tail) as Cons).tail as Cons).tail;
    ret = ret + ((((l as Cons).tail) as Cons).tail as Cons).head;    // 0+4+5
    _ceu_out_assert_msg(ret == 9, "2");

    await (((l as Cons).tail) as Cons).tail;
    ret = ret + ((((l as Cons).tail) as Cons).tail is Nil);          // 0+4+5+5+1
    _ceu_out_assert_msg(ret == 15, "4");
    await FOREVER;
with
    await (((l as Cons).tail) as Cons).tail;
    _ceu_out_assert_msg(ret == 9, "3");
    ret = ret + ((((l as Cons).tail) as Cons).tail as Cons).head;    // 0+4+5+5
    (((l as Cons).tail) as Cons).tail = new Nil();

    _ceu_out_assert_msg(ret == 15, "5");
    await (((l as Cons).tail) as Cons).tail;
    // never reached
    _ceu_out_assert_msg(ret == 15, "6");
    await FOREVER;
with
    await (((l as Cons).tail) as Cons).tail;
    ret = ret + ((((l as Cons).tail) as Cons).tail is Nil);          // 0+4+5+5+1+1

    await (((l as Cons).tail) as Cons).tail;
    _ceu_out_assert_msg(ret == 16, "7");
    await FOREVER;
with
    (((l as Cons).tail) as Cons).tail = ((((l as Cons).tail) as Cons).tail as Cons).tail;
    ret = ret * 2;  // (0+4+5+5+1+1) * 2
    (((l as Cons).tail) as Cons).tail = new Cons(10, Nil());
end

escape ret;
]],
    _ana = {acc=true},
    run = 32,
}

Test { [[
input void OS_START;

data Widget;
    data Nil is Wiget;
    data Vv is Widget with
        var int v;
    end
    data Row is Widget with
        var Widget  w1;
        var Widget  w2;
    end

par/or do
    await 21s;
with
    pool[] Widget widgets = new Row(
                    Vv(10),
                    Vv(20));

    var int ret = do/_
        traverse widget in &&widgets do
            watching *widget do
                if (*widget is Vv) then
                    await ((*widget as Vv).v)s;
                    escape (*widget as Vv).v;

                else/if (*widget is Row) then
                    var int v1=0, v2=0;
                    par/and do
                        v1 = traverse &&(*widget as Row).w1;
                    with
                        v2 = traverse &&(*widget as Row).w2;
                    end
                    escape v1 + v2;

                else
native _ceu_out_assert_msg;
                    _ceu_out_assert_msg(0, "not implemented");
                end
            end
            escape 0;
end
        end;
    escape ret;
end

escape 0;
]],
    _ana = {acc=true},
    wrn = true,
    run = {['~>21s;'] = 30},
}

Test { [[
input void OS_START;

data Widget;
    data Nil_;
    data Nil is Wiget;
    data Empty is Wiget;
    data Row is Widget with
        var Widget  w1;
        var Widget  w2;
    end

par/or do
    await OS_START;
with
    pool[] Widget widgets;
    widgets = new Row(
                    Empty(),
                    Empty());

    traverse widget in &&widgets do
        watching *widget do
            if (*widget is Nil) then
                await FOREVER;
            else/if (*widget is Empty) then
                escape 1;

            else/if (*widget is Row) then
                loop i in [0 |> 3[ do
                    par/or do
                        var int ret = traverse &&(*widget as Row).w1;
                        if ret == 0 then
                            await FOREVER;
                        end
                    with
                        var int ret = traverse &&(*widget as Row).w2;
                        if ret == 0 then
                            await FOREVER;
                        end
                    end
                end

            else
native _ceu_out_assert_msg;
                _ceu_out_assert_msg(0, "not implemented");
            end
        end
    end
end

escape 1;
]],
    _ana = {acc=true},
    wrn = true,
    run = 1,
}

Test { [[
input void OS_START;

data List;
    data Nil is List;
    data Empty is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[] List l = new Cons(1, Empty());

par/or do
    traverse e in &&l do
        watching *e do
            if (*e is Empty) then
                await FOREVER;

            else/if (*e is Cons) then
                loop do
                    traverse &&(*e as Cons).tail;
native _ceu_out_assert_msg;
                    _ceu_out_assert_msg(0, "0");
                end
            else
                _ceu_out_assert_msg(0, "1");
            end
        end
    end
with
    await OS_START;
end

escape 1;
]],
    _ana = {acc=true},
    wrn = true,
    run = 1,
}

Test { [[
input void OS_START;

data Widget;
    data Nil is Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

var int ret = 0;

par/or do
    pool[] Widget widgets;
    widgets = new Seq(
                Empty(),
                Empty());

    traverse widget in &&widgets with
        var int param = 1;
    do
        ret = ret + param;

        watching *widget do
            if (*widget is Empty) then
                await FOREVER;

            else/if (*widget is Seq) then
                loop do
                    par/or do
                        traverse &&(*widget as Seq).w1 with
                            this.param = param + 1;
                        end;
if ((*widget is Seq).w1 is Nil) then
    await FOREVER;
end
                    with
                        traverse &&(*widget as Seq).w2 with
                            this.param = param + 1;
                        end;
if ((*widget is Seq).w2 is Nil) then
    await FOREVER;
end
                    end
                end

            else
native _ceu_out_assert_msg;
                _ceu_out_assert_msg(0, "not implemented");
            end
        end
    end
with
    await OS_START;
end

escape ret;
]],
    _ana = { acc=true },
    wrn = 'line 57 : unbounded recursive spawn',
    run = 5,
}
Test { [[
input void OS_START;

data Widget;
    data Nil is Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

var int ret = 0;

par/or do
    pool[10] Widget widgets = new Seq(
                Empty(),
                Empty());

    traverse widget in &&widgets with
        var int param = 1;
    do
        ret = ret + param;

        watching *widget do
            if (*widget is Empty) then
                await FOREVER;

            else/if (*widget is Seq) then
                loop do
                    par/or do
                        traverse &&(*widget as Seq).w1 with
                            this.param = param + 1;
                        end;
if ((*widget is Seq).w1 is Nil) then
    await FOREVER;
end
                    with
                        traverse &&(*widget as Seq).w2 with
                            this.param = param + 1;
                        end;
if ((*widget is Seq).w2 is Nil) then
    await FOREVER;
end
                    end
                end

            else
native _ceu_out_assert_msg;
                _ceu_out_assert_msg(0, "not implemented");
            end
        end
    end
with
    await OS_START;
end

escape ret;
]],
    _ana = { acc=true },
    run = 5,
}

Test { [[
input void OS_START;

data Widget;
    data Nil is Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

pool[] Widget widgets;
widgets = new Seq(
            Empty(),
            Empty());

var int ret = 0;

par/or do
    traverse widget in &&widgets with
        var int param = 1;
    do
        ret = ret + param;

        watching *widget do
            if (*widget is Empty) then
                await FOREVER;

            else/if (*widget is Seq) then
                loop do
                    par/or do
                        traverse &&(*widget as Seq).w1 with
                            this.param = param + 1;
                        end;
if ((*widget is Seq).w1 is Nil) then
native _ceu_out_assert_msg;
_ceu_out_assert_msg(0, "ok\n");
    await FOREVER;
end
                    with
                        traverse &&(*widget as Seq).w2 with
                            this.param = param + 1;
                        end;
if ((*widget is Seq).w2 is Nil) then
_ceu_out_assert_msg(0, "ok\n");
    await FOREVER;
end
                    end
                end

            else
                _ceu_out_assert_msg(0, "not implemented");
            end
        end
    end
with
    await OS_START;
end

escape ret;
]],
    _ana = { acc=true },
    wrn = 'line 57 : unbounded recursive spawn',
    run = 5,
}
Test { [[
input void OS_START;

data Widget;
    data Nil is Widget;
    data Empty is Widget;
    data Seq is Widget with
        var Widget  w1;
        var Widget  w2;
    end

pool[10] Widget widgets = new Seq(
            Empty(),
            Empty());

var int ret = 0;

par/or do
    traverse widget in &&widgets with
        var int param = 1;
    do
        ret = ret + param;

        watching *widget do
            if (*widget is Empty) then
                await FOREVER;

            else/if (*widget is Seq) then
                loop do
                    par/or do
                        traverse &&(*widget as Seq).w1 with
                            this.param = param + 1;
                        end;
if ((*widget is Seq).w1 is Nil) then
native _ceu_out_assert_msg;
_ceu_out_assert_msg(0, "ok\n");
    await FOREVER;
end
                    with
                        traverse &&(*widget as Seq).w2 with
                            this.param = param + 1;
                        end;
if ((*widget is Seq).w2 is Nil) then
_ceu_out_assert_msg(0, "ok\n");
    await FOREVER;
end
                    end
                end

            else
                _ceu_out_assert_msg(0, "not implemented");
            end
        end
    end
with
    await OS_START;
end

escape ret;
]],
    _ana = { acc=true },
    run = 5,
}

Test { [[
data Command;
    data Nothing is Command;
    data Forward is Command with
        var int pixels;
    end
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end

pool[] Command cmds;

cmds = new Sequence(
            Forward(100),
            Forward(500));

par/or do
    traverse cmd in &&cmds do
do finalize with
end
        watching *cmd do
            if (*cmd is Forward) then
                await FOREVER;

            else/if (*cmd is Sequence) then
                traverse &&(*cmd as Sequence).one;

            else
            end
        end
    end
with
    await 100s;
end

escape 10;
]],
    --tight = 'tight loop',
    _ana = { acc=true },
    wrn = true,
    run = { ['~>100s']=10 },
}

Test { [[
data Command;
    data Nothing is Command;
    data Sequence is Command with
        var Command  one;
    end

pool[] Command cmds = new Sequence(Nothing());

par/or do
    traverse cmd in &&cmds do
        if (*cmd is Nothing) then
            await FOREVER;
        else/if (*cmd is Sequence) then
            traverse &&(*cmd as Sequence).one;
        end
    end
with
end

escape 10;
]],
    wrn = true,
    run = 10,
}

Test { [[
data Command;
    data Nothing is Command;
    data Forward is Command with
        var int pixels;
    end
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end

pool[] Command cmds;

cmds = new Sequence(
            Forward(100),
            Forward(500));

par/or do
    traverse cmd in &&cmds do
        watching *cmd do
            if (*cmd is Forward) then
                await FOREVER;

            else/if (*cmd is Sequence) then
                traverse &&(*cmd as Sequence).one;

            else
            end
        end
    end
with
    await 100s;
end

escape 10;
]],
    --tight = 'tight loop',
    _ana = { acc=true },
    wrn = true,
    run = { ['~>100s']=10 },
}

Test { [[
input int SDL_DT;

data Command;
    data Nothing is Command;
    data Forward is Command with
        var int pixels;
    end
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end

// TODO: aceitar estatico
pool[] Command cmds = new Sequence(
            Forward(100),
            Forward(500));

par/or do
    await 100s;
with
    traverse cmd in &&cmds do
        watching *cmd do
            if (*cmd is Forward) then
                await FOREVER;

            else/if (*cmd is Sequence) then
                traverse &&(*cmd as Sequence).one;
native _ceu_out_assert_msg;
                _ceu_out_assert_msg(0, "bug found"); // cmds has to die entirely before children
                traverse &&(*cmd as Sequence).two;
            end
        end
    end
end

escape 10;
]],
    --tight = 'tight loop',
    _ana = { acc=true },
    wrn = true,
    run = { ['~>100s']=10 },
}
Test { [[
input int SDL_DT;

data Command;
    data Nothing is Command;
    data Forward is Command with
        var int pixels;
    end
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end

// TODO: aceitar estatico
pool[] Command cmds;

cmds = new Sequence(
            Forward(100),
            Forward(500));

par/or do
    await 100s;
with
    traverse cmd in &&cmds do
        watching *cmd do
            if (*cmd is Forward) then
                await FOREVER;

            else/if (*cmd is Sequence) then
                traverse &&(*cmd as Sequence).one;
                traverse &&(*cmd as Sequence).two;
            end
        end
    end
end

escape 10;
]],
    --tight = 'tight loop',
    _ana = { acc=true },
    wrn = true,
    run = { ['~>100s']=10 },
}

Test { [[
data Command;
    data Nothing is Command;
    data Left;
    data Repeat is Command with
        var Command  command;
    end

pool[] Command cmds = new Repeat(
            Left());

class TurtleTurn with
do
    await 1us;
end

traverse cmd in &&cmds do
    watching *cmd do
        if (*cmd is Left) then
            do TurtleTurn;

        else/if (*cmd is Repeat) then
            traverse &&(*cmd as Repeat).command;
            traverse &&(*cmd as Repeat).command;

        else
native _ceu_out_assert_msg;
            _ceu_out_assert_msg(0, "not implemented");
        end
    end
end

escape 10;
]],
    --tight = 'tight loop',
    _ana = { acc=true },
    wrn = true,
    run = { ['~>2us']=10 },
}

Test { [[
input int SDL_DT;

data Command;
    data Nothing is Command;
    data Await is Command with
        var int ms;
    end
    data Right is Command with
        var int angle;
    end
    data Left is Command with
        var int angle;
    end
    data Forward is Command with
        var int pixels;
    end
    data Backward is Command with
        var int pixels;
    end
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end
    data Repeat is Command with
        var int      times;
        var Command  command;
    end

// TODO: aceitar estatico
pool[] Command cmds;

cmds = new Repeat(2,
            Sequence(
                Await(500),
                Sequence(
                    Right(45),
                    Sequence(
                        Forward(100),
                        Sequence(
                            Left(90),
                            Sequence(
                                Forward(100),
                                Sequence(
                                    Right(45),
                                    Sequence(
                                        Backward(100),
                                        Await(500)))))))));

class Turtle with
    var int angle=0;
    var int pos_x=0, pos_y=0;
do
    await FOREVER;
end

class TurtleTurn with
    var& Turtle turtle;
    var int     angle;
    var int     isRight;
do
    var int inc;
    if isRight then
        if this.angle < 0 then
            angle = -angle;
            inc = 1;
        else
            inc = -1;
        end
    else
        if this.angle < 0 then
            angle = -angle;
            inc = -1;
        else
            inc = 1;
        end
    end
    loop i in [0|>angle[ do
        await 10ms;
        turtle.angle = turtle.angle + inc;
    end
end

class TurtleMove with
    var& Turtle turtle;
    var int     pixels;
    var int     isForward;
do
    var int inc;
    if isForward then
        inc =  1;
    else
        inc = -1;
    end
native _ceu_out_assert_msg;
    _ceu_out_assert_msg(this.pixels > 0, "pixels");

    var float sum = 0;
    var float x = turtle.pos_x;
    var float y = turtle.pos_y;
    loop do
        await 10ms;
        var int dt = 10;
        if sum >= this.pixels then
            break;
        end
        var float mul = 80 * dt * 0.001 * this.inc;
        var float dx  = mul * (turtle.angle/(180.0));
        var float dy  = mul * (turtle.angle/(180.0));
        sum = sum + (dx) + (dy);
        x = x + dx;
        y = y + dy;
        turtle.pos_x = x;
        turtle.pos_y = y;
    end

end

par/or do
    await 100s;
with
    var Turtle turtle;

    traverse cmd in &&cmds do
        watching *cmd do
            if (*cmd is Await) then
                await ((*cmd as Await).ms) ms;

            else/if (*cmd is Right) or (*cmd is Left) then
                var int angle;
                if (*cmd is Right) then
                    angle = (*cmd as Right).angle;
                else
                    angle = (*cmd as Left).angle;
                end
                do TurtleTurn with
                    this.turtle  = &turtle;
                    this.angle   = angle;
                    this.isRight = (*cmd is Right);
                end;

            else/if (*cmd is Forward) or (*cmd is Backward) then
                var int pixels;
                if (*cmd is Forward) then
                    pixels = (*cmd as Forward).pixels;
                else
                    pixels = (*cmd as Backward).pixels;
                end
                do TurtleMove with
                    this.turtle    = &turtle;
                    this.pixels    = pixels;
                    this.isForward = (*cmd is Forward);
                end;

            else/if (*cmd is Sequence) then
                traverse &&(*cmd as Sequence).one;
                traverse &&(*cmd as Sequence).two;

            else/if (*cmd is Repeat) then
                loop i in (*cmd as Repeat).times do
                    traverse &&(*cmd as Repeat).command;
                end

            else
                _ceu_out_assert_msg(0, "not implemented");
            end
        end
    end
end

escape 10;
]],
    --tight = 'tight loop',
    _ana = { acc=true },
    wrn = true,
    run = { ['~>100s']=10 },
}

-- creates a loop when reusing address of organisms being killed
Test { [[
data Command;
    data Nothing is Command;
    data Await is Command with
        var int ms;
    end
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end
    data Repeat is Command with
        var int      times;
        var Command  command;
    end

pool[] Command cmds;

cmds = new Repeat(2,
            Sequence(
                Await(100),
                Sequence(
                    Await(100),
                    Await(500))));

var int ret = 0;

traverse cmd in &&cmds do
    watching *cmd do
        if (*cmd is Await) then
            await ((*cmd as Await).ms) ms;
            ret = ret + 1;

        else/if (*cmd is Sequence) then
            ret = ret + 2;
            traverse &&(*cmd as Sequence).one;
            traverse &&(*cmd as Sequence).two;

        else/if (*cmd is Repeat) then
            loop i in [0|>(*cmd as Repeat).times[ do
                ret = ret + 3;
                traverse &&(*cmd as Repeat).command;
            end

        else
native _ceu_out_assert_msg;
            _ceu_out_assert_msg(0, "not implemented");
        end
    end
end

escape ret;
]],
    wrn = true,
    _ana = {acc=true},
    run = { ['~>100s']=20 },
}

Test { [[
native/nohold _free;
var& void? ptr;
    do ptr = &_malloc(10000);
finalize with
    _free(&&ptr!);
end

data Command;
    data Nothing is Command;
    data Await is Command with
        var int ms;
    end
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end
    data Repeat is Command with
        var int      times;
        var Command  command;
    end

// TODO: aceitar estatico
pool[] Command cmds = new Repeat(2,
            Sequence(
                Await(100),
                Sequence(
                    Await(300),
                    Await(500))));

var int ret = 0;

traverse cmd in &&cmds do
    watching *cmd do
        if (*cmd is Await) then
            await ((*cmd as Await).ms) ms;
            ret = ret + 1;

        else/if (*cmd is Sequence) then
            ret = ret + 2;
            traverse &&(*cmd as Sequence).one;
            traverse &&(*cmd as Sequence).two;

        else/if (*cmd is Repeat) then
            loop i in [0|>(*cmd as Repeat).times[ do
                ret = ret + 3;
                traverse &&(*cmd as Repeat).command;
            end

        else
native _ceu_out_assert_msg;
            _ceu_out_assert_msg(0, "not implemented");
        end
    end
end

escape ret;
]],
    wrn = true,
    _ana = {acc=true},
    run = { ['~>100s']=20 },
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var List tail;
    end

pool[] List ls;
ls = new Cons(Nil());

traverse l in &&ls do
    if (*l is Nil) then
        await FOREVER;
    else
        watching *l do
            par/or do
                traverse &&((l as Cons).tail);
            with
                await 1s;
            end
        end
    end
end

escape 1;
]],
    wrn = 'line 23 : unbounded recursive spawn',
    run = { ['~>5s']=1 },
}
Test { [[
data List;
    data Nil is List;
    data Hold is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[] List ls;
ls = new Cons(1,
            Cons(2,
                Hold()));

var int ret = 0;

traverse l in &&ls do
    ret = ret + 1;
    watching *l do
        if (*l is Hold) then
            do finalize with
                ret = ret + 1;
            end
            await FOREVER;
        else
            par/or do
                traverse &&((l as Cons).tail);
            with
                await 1s;
            end
        end
    end
end

escape ret;
]],
    wrn = 'line 23 : unbounded recursive spawn',
    run = { ['~>5s']=4 },
}
Test { [[
data List;
    data Nil is List;
    data Hold is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[] List ls;
ls = new Cons(1,
            Cons(2,
                Hold()));

var int ret = 0;

do
    traverse l in &&ls do
        ret = ret + 1;
        watching *l do
            if (*l is Hold) then
                do finalize with
                    ret = ret + 1;
                end
                await FOREVER;
            else
                par/or do
                    traverse &&((l as Cons).tail);
                with
                    await 1s;
                end
            end
        end
    end
end

escape ret;
]],
    wrn = 'line 23 : unbounded recursive spawn',
    run = { ['~>5s']=4 },
}
Test { [[
data List;
    data Nil is List;
    data Hold is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[10] List ls = new Cons(1,
            Cons(2,
                Hold()));

var int ret = 0;

traverse l in &&ls do
    ret = ret + 1;
    watching *l do
        if *l is Hold then
            do finalize with
                ret = ret + 1;
            end
            await FOREVER;
        else
            par/or do
                traverse &&((l as Cons).tail);
            with
                await 1s;
            end
        end
    end
end

escape ret;
]],
    run = { ['~>5s']=4 },
}
Test { [[
data List;
    data Nil is List;
    data Hold is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[10] List ls = new Cons(1,
            Cons(2,
                Hold()));

var int ret = 0;

do
    traverse l in &&ls do
        ret = ret + 1;
        watching *l do
            if *l is Hold then
                do finalize with
                    ret = ret + 1;
                end
                await FOREVER;
            else
                par/or do
                    traverse &&((l as Cons).tail);
                with
                    await 1s;
                end
            end
        end
    end
end

escape ret;
]],
    run = { ['~>5s']=4 },
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[10] List list;

var int i = 10;
traverse l in &&list do
    list = new Cons(i, Nil());
end

escape 1;
]],
    run = 1,
}
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[10] List list;

loop i in [0 |> 10[ do
    traverse l in &&list do
        if (*l is Nil) then
            list = new Cons(i, Nil());
        else/if (*l as Cons) then
            if ((l as Cons).tail) is Nil then
                ((l as Cons).tail) = new Cons(i, Nil());
            else
                traverse &&((l as Cons).tail);
            end
        end
    end
end

var int sum = 0;

traverse l in &&list do
    if (*l as Cons) then
        sum = sum + ((l as Cons).head);
        traverse &&((l as Cons).tail);
    end
end

escape sum;
]],
    run = 45,
}

-- innefective Nil inside watching
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

native do
    int V = 0;
end

class Body with
    pool&[]  Body bodies;
    var   List&&   n;
do
    watching *n do
        if (*n is Nil) then
native _V;
            _V = _V * 2;
        else/if (*n is Cons) then
            _V = _V + 1;
            _V = _V + (*n is Cons).head;

            var Body&&? tail =
                spawn Body in this.bodies with
                    this.bodies = bodies;
                    this.n      = &&(*n is Cons).tail;
                end;
            if tail? then
                await *tail!;
            end
        end
    end
end

pool[3] Body bodies;
do Body with
    this.bodies = bodies;
    this.n      = &&list;
end;

escape _V;
]],
    adt = 'line 24 : ineffective use of data "Nil" due to enclosing `watching´',
}
Test { [[
data List;
    data Nil_;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[4] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

native do
    int V = 0;
end

class Body with
    pool&[]  Body bodies;
    var   List&&   n;
do
    watching *n do
        if (*n is Nil) then
native _V;
            _V = _V * 2;
        else/if (*n is Cons) then
            _V = _V + 1;
            _V = _V + (*n is Cons).head;

            var Body&&? tail =
                spawn Body in this.bodies with
                    this.bodies = &bodies;
                    this.n      = &&(*n is Cons).tail;
                end;
            if tail? then
                await *tail!;
            end
        end
    end
end

pool[4] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&list;
end;

escape _V;
]],
    wrn = 'line 42 : unbounded recursive spawn',
    _ana = { acc=true },
    run = 18,
}
-- innefective Nil inside watching
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

native do
    int V = 0;
end

traverse n in &&list do
native _V;
    _V = _V + 1;
    watching *n do
        if (*n is Nil) then
            _V = _V * 2;
        else/if (*n is Cons) then
            _V = _V + (*n is Cons).head;
            traverse &&(*n is Cons).tail;
        end
    end
    await 1s;
end

escape _V;
]],
    adt = 'line 22 : ineffective use of data "Nil" due to enclosing `watching´',
}

Test { [[
data List;
    data Nil_ is List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[4] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

native do
    int V = 0;
end

traverse n in &&list do
native _V;
    _V = _V + 1;
    watching *n do
        if (*n is Nil) then
            _V = _V * 2;
        else/if (*n is Cons) then
            _V = _V + (*n is Cons).head;
            traverse &&(*n is Cons).tail;
        end
    end
    await 1s;
end

escape _V;
]],
    wrn = 'line 42 : unbounded recursive spawn',
    _ana = { acc=true },
    run = { ['~>10s']=20 },
}

Test { [[
data List;
    data Nil is List;
    data Val is List with
        var List  l;
    end

pool[] List ls;

var int v = 10;
var int&& p = &&v;

traverse l in &&ls do
    await 1s;
    *p = 1;
    if *l is Val then
        traverse &&(*l as Val).l;
    end
end

escape v;
]],
    fin = 'line 16 : unsafe access to pointer "p" across `await´',
}

Test { [[
class A with
    var int v = 10;
    var int&& p;
do
    this.p = &&v;
end

class B with
    var& A a;
do
    escape *(a.p);
end

escape 1;
]],
    fin = 'line 11 : unsafe access to pointer "p" across `class´ (tests.lua : 8)',
    --run = 1,
}

Test { [[
class A with
    var int v = 10;
    var int&& p;
do
    this.p = &&v;
end

class B with
    var& A a;
do
    await 1s;
    escape *(a.p);
end

escape 1;
]],
    fin = 'line 12 : unsafe access to pointer "p" across `class´',
}

Test { [[
data Stmt;
    data Nil is Stmt;
    data Seq is Stmt with
        var Stmt s1;
    end

pool[2] Stmt stmts = new Nil();

var int ddd = do/_
    traverse stmt in &&stmts do
        escape 10;
end
    end;

escape ddd;
]],
    wrn = true,
    run = 10,
}

Test { [[
data Stmt;
    data Nil is Stmt;
    data Seq is Stmt with
        var Stmt s1;
    end

pool[] Stmt stmts = new Nil();

var int v1 = 10;

var int ret = do/_
    traverse stmt in &&stmts with
        var int v2 = v1;
    do
        escape v1+v2;
end
    end;

escape ret;
]],
    wrn = true,
    run = 20,
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list = new
    Cons(1,
        Cons(2,
            Cons(3,
                Nil())));

var int s1 = do/_
    traverse l in &&list do
        if (*l is Nil) then
            escape 0;
        else
            watching *l do
                var int sum_tail = traverse &&((l as Cons).tail);
                escape sum_tail + ((l as Cons).head);
            end
        end
end
    end;

escape s1;
]],
    run = 6,
}

Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list = new
    Cons(1,
        Cons(2,
            Cons(3,
                Nil())));

var int s2 = 0;
var int s1 = do/_
    traverse l in &&list do
        if (*l is Nil) then
            escape 0;
        else
            watching *l do
                var int sum_tail = traverse &&((l as Cons).tail);
                s2 = s2 + ((l as Cons).head);
                escape sum_tail + ((l as Cons).head);
            end
        end
end
    end;

escape s1 + s2;
]],
    run = 12,
}

Test { [[
data Tx with
    data Nil is List;
or
    data Next is Command with
        var Tx  next;
    end
end

class C with
    var int v;
do
    pool[] Tx ts; // = new Tx.Next(Tx.Nil());
    var int ret = do/_
        traverse t in ts do
            escape this.v;
end
        end;
    escape ret;
end

var int v =
    do C with
        this.v = 10;
    end;

escape v;
]],
    todo = true,
    run = 10,
}

Test { [[
data Command;
    data Nothing is Command;
    data Await is Command with
        var int ms;
    end
    data Stream_Root is Command with
        var Command  run;
        var Command  now;
        var Command  nxt;
    end
    data Stream_Next is Command with
        var Command  one;
        var Command  two;
    end
    data Stream_End;

pool[100] Command cmds = new
    Stream_Root(
        Await(1000),
        Stream_End(),
        Stream_End());

                (cmds as Stream_Root).nxt = new Stream_End();

    traverse cmd in &&cmds do
        watching *cmd do
            if (*cmd is Await) then
                await ((*cmd as Await).ms) ms;

            else/if (*cmd is Stream_Root) then
                (cmds as Stream_Root).nxt = new Stream_End();
            end
        end
    end

escape 1;
]],
    _ana = {acc=true},
    run = 1,
}

Test { [[
native/plain _SDL_Renderer;
native/nohold _f;
class Turtle with
    var& _SDL_Renderer ren;
do
    every 1s do
        _f(&&ren);
    end
end
escape 1;
]],
    gcc = '5: error: implicit declaration of function ‘f’',
}

Test { [[
data Command;
    data Nothing is Command;
    data Stream_Root is Command with
        var Command  run;
        var Command  now;
        var Command  nxt;
    end
    data Stream_Next is Command with
        var Command  one;
        var Command  two;
    end
    data Stream_End;

pool[] Command cmds;
((cmds as Stream_Root).now as Stream_Next).one = (cmds as Stream_Root).nxt;
(cmds as Stream_Root).run = ((cmds as Stream_Root).nxt as Stream_Next).two;
traverse cmd in &&cmds do
    ((*cmd as Stream_Root).now as Stream_Next).one = (*cmd as Stream_Root).nxt;
    (*cmd as Stream_Root).run = ((*cmd as Stream_Root).nxt as Stream_Next).two;
    ((cmds as Stream_Root).now as Stream_Next).one = (cmds as Stream_Root).nxt;
    (cmds as Stream_Root).run = ((cmds as Stream_Root).nxt as Stream_Next).two;
end
((cmds as Stream_Root).now as Stream_Next).one = (cmds as Stream_Root).now;
escape 1;
]],
    adt = 'line 27 : cannot assign parent to child',
}

Test { [[
data Val with
    var int v;
end

data Exp;
    data Nil is Exp;
    data Val is Exp with
        var Val v;
    end
    data Add is Exp with
        var Exp e1;
        var Exp e2;
    end

data Stmt;
    data Nil is Stmt;
    data Seq is Stmt with
        var Stmt s1;
        var Stmt s2;
    end
    data Print is Stmt with
        var Exp e;
    end

pool[] Stmt stmts =
    new Seq(
            Print(
                Add(
                    Val(Val(10)),
                    Val(Val(5)))),
            Nil());

traverse stmt in stmts do
    if *stmt is Seq then
        traverse &&(*stmt as Seq).s1;
    else/if *stmt is Print then
        var int v = traverse &&(*stmt as Print).e;
    end
end

escape 1;
]],
    adt = 'line 43 : invalid attribution : reference : types mismatch (`Stmt´ <= `Exp´)',
}

-- par/or kills (2) which should be aborted
Test { [[
data Exp;
    data Nil is Exp;
    data Vv is Exp with
        var int e;
    end
    data Add is Exp with
        var Exp e1;
        var Exp e2;
    end

var int ret = 0;

pool[] Exp exps = new
    Add(Nil(), Vv(20));

traverse e in &&exps do
    watching *e do
        if *e is Add then
            ret = ret + 1;
            par/or do
                traverse &&(*e as Add).e2;
            with
                traverse &&(*e as Add).e1;
            end
            await 5s;
        else/if *e is Vv then
            every 1s do
                ret = ret + (*e as Vv).e;
            end
        end
    end
end

escape ret;
]],
    _ana = {acc=true},
    wrn = true,
    run = { ['~>10s'] = 1 },
}

Test { [[
data Exp;
    data Nil is Exp;
    data Sub is Exp with
        var Exp e2;
    end

data Stmt;
    data Nil is Stmt;
    data Seq is Stmt with
        var Stmt s1;
        var Exp e;
    end

    pool[] Stmt stmts;
    traverse stmt in &&stmts do
        watching *stmt do
        end
    end
escape 1;
]],
    _ana = {acc=true},
    wrn = true,
    run = 1,
}

-- << DATA / RECURSE / TRAVERSE

-->> TRAVERSE / NUMERIC

Test { [[
var int tot = 4;
var int ret = do/_
    traverse idx in [] do
        if idx == tot then
            escape idx;
        else
            var int ret = traverse idx+1;
            escape idx + ret;
        end
end
    end;

escape ret;
]],
    wrn = true,
    run = 10,
}

Test { [[
var int tot = 4;
var int ret = do/_
    traverse idx in [3] do
        if idx == tot then
            escape idx;
        else
            var int ret = traverse idx+1;
            escape idx + ret;
        end
end
    end;

escape ret;
]],
    run = 3,
}

Test { [[
loop v in [0 |> 10[ do
    traverse 1;
end
]],
    adj = 'line 2 : missing enclosing `traverse´ block',
}

Test { [[
traverse v in [10] do
    traverse 1;
end
escape 1;
]],
    run = 1,
}

--<< TRAVERSE / NUMERIC

-->> TRAVERSE / NESTED-RECURSIVE-DATA
Test { [[
data List;
    data Nil is List;
    data Cons is List with
        var int   head;
        var List  tail;
    end

pool[3] List list;
list = new Cons(1,
            Cons(2,
                Cons(3, Nil())));

var int sum = 0;

pool[] List&& lll = &&(list as Cons).tail;

traverse n in lll do
    sum = sum + 1;
    if (*n is Cons) then
        sum = sum + (*n is Cons).head;
        traverse &&(*n is Cons).tail;
    end
end

escape sum;
]],
    wrn = true,
    run = 8,
}

Test { [[
data Xx;
    data Nil is Xx;
    data Nil is Xx;

escape 1;
]],
    tops = 'line 3 : identifier "Nil" is already declared (tests.lua : line 2)',
    --env = 'line 4 : duplicated data : "Nil"',
}

Test { [[
data Command;
    data Nothing is Command;
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end

data CommandQueue;
    data Nothing is CommandQueue;
    data Nxt with
        var Command       cmd;
        var CommandQueue  nxt;
    end

escape 1;
]],
    --run = 1,
    --env = 'line 13 : duplicated data : "Nothing"',
    tops = 'line 9 : identifier "Nothing" is already declared (tests.lua : line 2)',
}

Test { [[
data Command;
    data Nothing is Command;
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end

data CommandQueue;
    data Nil is CommandQueue;
    data Nxt with
        var Command       cmd;
        var CommandQueue  nxt;
    end

pool[10] CommandQueue cq1 = new Nothing();
pool[10] CommandQueue cq2 = new Nil();

pool[10] CommandQueue cq3 = new
    Nxt(
        Sequence(
            Nothing(),
            Nothing()),
        Nil());

escape 1;
]],
    run = 1,
}

Test { [[
data Command;
    data Nothing is Command;
    data Sequence is Command with
        var Command  one;
        var Command  two;
    end

data CommandQueue;
    data Nil is CommandQueue;
    data Nxt with
        var Command       cmd;
        var CommandQueue  nxt;
    end

pool[10] CommandQueue cq1 = new Nil();

pool[10] CommandQueue cq2 = new
    Nxt(
        Sequence(
            Nothing(),
            Nothing()),
        Nil());

escape (cq1 is Nil) + (((cq2 as Nxt).cmd as Sequence).one is Nothing);
]],
    run = 2,
}
--<< TRAVERSE / NESTED-RECURSIVE-DATA

-- DATA ALIASING

Test { [[
data List;
    data Nil is List;
    data Xx with
        var List  nxt;
    end
pool[] List lll;     // l is the pool
escape lll is Nil;       // l is a pointer to the root
]],
    wrn = true,
    run = 1,
}

Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[] Command cmds1;
pool&[] Command cmds2;
escape 1;
]],
    wrn = true,
    ref = 'line 10 : uninitialized variable "cmds2" crossing compound statement (tests.lua:1)',
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[] Command cmds1;
pool&[] Command cmds2=&cmds1;
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[] Command cmds1;
pool&[] Command cmds2;
cmds2 = &cmds1;
escape 1;
]],
    wrn = true,
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[] Command cmds1;
cmds1 = new Next(
            Next(
                Nothing()));

pool&[] Command cmds2 = &cmds1;

escape (((cmds2 as Next).nxt as Next).nxt is Nothing);
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool&[] Command cmds2
    = &new Next(
            Next(
                Nothing()));

escape 1;
]],
    parser = 'line 8 : after `&´ : expected expression',
    --parser = 'line 10 : after `new´ : expected `;´'
    --ref = 'line 9 : invalid attribution (not a reference)',
    --ref = 'line 10 : reference must be bounded before use',
}

Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[2] Command cmds1;
pool&[2] Command cmds2
        = &cmds1;

cmds1 = new Next(
            Next(
                Nothing()));
cmds1 = new Nothing();
cmds2 = new Next(
            Next(
                Nothing()));
escape cmds1 is Next;
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[2] Command cmds1;
pool&[2] Command cmds2
        = &cmds1;

cmds1 = new Next(
            Next(
                Nothing()));
cmds2 = new Next(
            Next(
                Nothing()));
escape cmds1 is Nothing;
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[2] Command cmds1;

cmds1 = new Next(
                Next(
                    Next(
                        Nothing())));
escape (((cmds1 as Next).nxt as Next).nxt is Nothing);
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[2] Command cmds1;

cmds1 = new Next(Nothing());
(cmds1 as Next).nxt = new Next(Nothing());
((cmds1 as Next).nxt as Next).nxt = new Next(Nothing());
escape (((cmds1 as Next).nxt as Next).nxt is Nothing);
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[1] Command cmds1;

cmds1 = new Next(Nothing());
cmds1 = new Nothing();
cmds1 = new Next(Nothing());
escape ((cmds1 as Next).nxt is Nothing);
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[1] Command cmds1;

cmds1 = new Next(Nothing());
cmds1 = new Next(Nothing());
escape cmds1 is Nothing;
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[2] Command cmds1 = new Next(Nothing());
cmds1 = new Nothing();
cmds1 = new Next(
                Next(
                    Nothing()));
escape (((cmds1 as Next).nxt as Next).nxt is Nothing);
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[2] Command cmds1 = new Next(Nothing());
cmds1 = new Next(
                Next(
                    Nothing()));
escape (cmds1 as Next).nxt is Nothing;
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[2] Command cmds1;
pool&[2] Command cmds2 = &cmds1;

cmds1 = new Next(Nothing());
(cmds2 as Next).nxt = new Next(Nothing());
escape (((cmds1 as Next).nxt as Next).nxt is Nothing);
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[2] Command cmds1;
pool&[2] Command cmds2 = &cmds1;

cmds1 = new Next(Nothing());
(cmds2 as Next).nxt = new Next(
                        Next(
                            Nothing()));
escape ((cmds1 as Next).nxt as Next).nxt is Nothing;
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[3] Command cmds1;
pool&[3] Command cmds2;
cmds2 = &cmds1;

cmds1 = new Next(
            Next(
                Nothing()));
((cmds2 as Next).nxt as Next).nxt = new Next(
                            Next(
                                Nothing()));
escape (((cmds1 as Next).nxt as Next).nxt as Next).nxt is Nothing;
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[10] Command cmds1;

pool&[10] Command cmds2;
cmds2 = &cmds1;

cmds1 = new Next(
            Next(
                Nothing()));
cmds2 = new Next(
            Next(
                Nothing()));

escape ((cmds1 as Next).nxt as Next).nxt is Nothing;
]],
    run = 1,
}
Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[] Command cmds1;

pool&[] Command cmds2 = &cmds1;

cmds1 = new Next(
            Next(
                Nothing()));
cmds2 = new Next(
            Next(
                Nothing()));

escape ((cmds1 as Next).nxt as Next).nxt is Nothing;
]],
    run = 1,
}

Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

pool[] Command cmds1;

pool&[] Command cmds2;
cmds2 = &cmds1;

cmds1 = new Next(
            Next(
                Nothing()));
cmds2 = new Next(
            Next(
                Nothing()));

var int sum = 0;

traverse cmd in &&cmds1 do
    if (*cmd is Next) then
        sum = sum + 1;
        traverse &&(*cmd as Next).nxt;
    end
end
traverse cmd in &&cmds2 do
    if (*cmd is Next) then
        sum = sum + 1;
        traverse &&(*cmd as Next).nxt;
    end
end

escape sum;
]],
    wrn = true,
    run = 4,
}

Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

class Run with
    pool&[] Command cmds1;
do
    cmds1 = new Next(
                Next(
                    Nothing()));
    var int sum = 0;
    traverse cmd111 in &&cmds1 do
        if *cmd111 is Next then
            sum = sum + 1;
            traverse &&(*cmd111 as Next).nxt;
        end
    end
    escape sum;
end

pool[] Command cmds;

traverse cmd222 in &&cmds do
end

var int ddd = do Run with
    this.cmds1 = &cmds;
end;

escape ddd;
]],
    wrn = true,
    run = 2,
}

Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

    pool[] Command cmds;
    traverse cmd in &&cmds do
    end

escape 1;
]],
    wrn = true,
    run = 1,
}

Test { [[
data Command;
    data Nothing is Command;
    data Next is Command with
        var Command  nxt;
    end

class Run with
    pool&[] Command cmds;
do
    var int sum = 0;
    traverse cmd in &&cmds do
        if (*cmd is Next) then
            sum = sum + 1;
            traverse &&(*cmd as Next).nxt;
        end
    end
    escape sum;
end

pool[] Command cmds = new Next(
            Next(
                Nothing()));

var int ret = do Run with
    this.cmds = &cmds;
end;

escape ret;
]],
    wrn = true,
    run = 2,
}

Test { [[
data Command;
    data Nothing is Command;
    data Await is Command;
    data ParOr is Command with
        var Command  one;
    end

pool[] Command cmds;

cmds = new ParOr(
            ParOr(
                Await()));

class Run with
    pool&[] Command cmds;
do
    traverse cmd in &&cmds do
        if (*cmd is Await) then
            await 1ms;

        else/if (*cmd is ParOr) then
            par/or do
                traverse &&(*cmd as ParOr).one;
            with
            end
        end
    end
end

var Run r with
    this.cmds   = &cmds;
end;

escape 1;
]],
    wrn = true,
    run = { ['~>10s']=1 },
}

Test { [[
data Dummy;
  data Nil is Dummy;
  data Rec is Dummy with
    var Dummy  rec;
  end

native do
    byte vec[3] = {5,5,5};
end

pool[] Dummy ds;

var int xxx = 0;

input void OS_START;

traverse d in &&ds with
    var int idx = 0;
do
    if idx < 3 then
        par/and do
            await OS_START;
            _vec[xxx] = idx;
            xxx = xxx + 1;
        with
            traverse d with
                this.idx = idx + 1;
            end;
        end
    end
end

escape (_vec[0]==0) + (_vec[1]==1) + (_vec[2]==2);
]],
    wrn = true,
    run = 3,
}

Test { [[
data NoRec;
    data Nil is NoRec;
    data Seq is NoRec with
        var int x;
    end

pool&[] NoRec norec;

traverse t in this.norec do
end

escape 1;
]],
    env = 'line 8 : invalid pool : non-recursive data',
}

Test { [[
data BTree;
    data Nil is BTree;
    data Seq is BTree with
        var BTree  nxt;
    end

class BTreeTraverse with
    pool&[3] BTree btree;
    var int x;
do
    var int a = this.x;
    pool&[3] BTree btree2 = &this.btree;
    traverse t in &&this.btree do
        var int a = this.x;
        if a then end;
    end
    escape 1;
end

escape 1;
]],
    run = 1,
}

Test { [[
data BTree;
    data Nil is BTree;
    data Seq is BTree with
        var BTree  nxt;
    end

pool[3] BTree bs = new Seq(Seq(Nil()));

class BTreeTraverse with
    pool&[3] BTree btree;
do
    var int ret = 0;
    traverse t in &&this.btree do
        if t is Seq then
            ret = ret + 1;
            traverse &&(*t as Seq).nxt;
        end
    end
    escape ret;
end

var int ret = do BTreeTraverse with
                    this.btree = &bs;
              end;

escape ret;
]],
    run = 2,
}

--[=[

Test { [[
data List;
    data Nil;
with
    data Cons is List with
        var int  head;
        var List tail;
    end
end
var List l = Cons(1, Cons(2, Cons(3, Nil())));

var int sum = 0;

loop/1 i in &&l do
    if i:Cons then
        sum = sum + i:Cons.head;
        traverse &&i:Cons.tail;
    end
end

escape sum;
]],
    asr = 'runtime error: loop overflow',
}

Test { [[
data List;
    data Nil;
with
    data Cons is List with
        var int  head;
        var List tail;
    end
end
var List l = Cons(1, Cons(2, Cons(3, Nil())));

var int sum = 0;

loop/3 i in &&l do
    if i:Cons then
        sum = sum + i:Cons.head;
        traverse &&i:Cons.tail;
    end
end

escape sum;
]],
    run = 6,
}

Test { [[
data List;
    data Nil;
with
    data Cons is List with
        var int  head;
        var List tail;
    end
end
var List l = Cons(1, Cons(2, Cons(3, Nil())));

var int sum = 0;

loop i in &&l do
    if i:Cons then
        sum = sum + i:Cons.head;
        traverse &&i:Cons.tail;
    end
end

escape sum;
]],
    run = 6,
}

Test { [[
data Tree;
    data Nil;
with
    data Node with
        var int   v;
        var Tree  left;
        var Tree  right;
    end
end

var Tree t =
    Node(1,
        Node(2, Nil(), Nil()),
        Node(3, Nil(), Nil()));

var int sum = 0;

    finalize with end;

loop i in &&t do
    if (*i is Node) then
        traverse &&(*i as Node).left;
        sum = sum + (*i as Node).v;
        traverse &&(*i as Node).right;
    end
end

escape sum;
]],
    run = 6,
}

Test { [[
data List;
    data Nil;
with
    data Cons is List with
        var int  head;
        var List tail;
    end
end

pool[3] List l;
l = new Cons(1, Cons(2, Cons(3, Nil())));

var int sum = 0;

loop i in l do
    if i:Cons then
        sum = sum + i:Cons.head;
        traverse &&i:Cons.tail;
    end
end

escape sum;
]],
    run = 6,
}

Test { [[
data Tree;
    data Nil;
with
    data Node with
        var int   v;
        var Tree  left;
        var Tree  right;
    end
end

pool[3] Tree t;
t = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 0;

loop i in t do
    if (*i is Node) then
        traverse &&(*i as Node).left;
        sum = sum + (*i as Node).v;
        traverse &&(*i as Node).right;
    end
end

escape sum;
]],
    run = 6,
}

]=]

-- TODO: continue ADT implementation

--[=[
-- XXX
-- TODO: avoid cycles/side-shares
error 'TODO: data that uses data'
error 'TODO: data that uses data that uses 1st data again (cycle)'
error 'TODO: detect tight loops == detect deletes in the DAG'
error 'TODO: change middle w/ l3 w/ deeper scope'
error 'TODO: List& l = ...  // for temporary parts (tests w/ no reassign)'

-- NONE of below is implemented (or will ever be?)
-- anonymous fields
Test { [[
data Pair = (int, int);
data Opt  = Nil(void) | Ptr(void*);
data List = Nil  (void)
           | Cons (int, List&);
escape 1;
]],
    todo = 'implement? trying only w/ named fields for now',
}

-- named fields w/ initializers
Test { [[
data Pair with
    var int x = 0;
    var int y = 0;
end

data Opt with
    data Nil;
with
    data Ptr with
        var void* v = null;
    end
end

data List;
    data Nil;
with
    data Cons is List with
        var int   head = 0;
        var& List tail = List.nil();
    end
end

var List l;

escape 1;
]],
    todo = 'implement?',
}

-- constructors may specify the field names
Test { DATA..[[
var Pair p1 = Pair(x=1,x=2);
var Opt  o1 = Opt.Nil();
var Opt  o2 = Opt.Ptr(v=&p1);
var List l1 = Nil();
var List l2 = Cons(head=1, tail=l1);
var List l3 = Cons(head=1, (tail as Cons).head)=2, tail=Nil()));

escape 1;
]],
    todo = 'implement?',
    run = 1,
}

-- anonymous destructors / pattern matching
Test { DATA..[[
var Pair p1 = Pair(1,2);
var Opt  o1 = Opt.Nil();
var Opt  o2 = Opt.Ptr(&p1);
var List l1 = Nil();
var List l2 = Cons(1, l1);
var List l3 = Cons(1, Cons(2, Nil()));

var int ret = 0;

var int x, y;
(x,y) = p;
_assert(x+y == 3);
ret = ret + 3;              // 3

switch o1 with
    case Opt.Nil() do
        ret = ret + 1;      // 4
        _assert(1);
    end
    case Opt.Ptr(void* v) do
        _assert(0);
    end
end

switch o2 with
    case Opt.Nil() do
        _assert(0);
    end
    case Opt.Ptr(void* v) do
        ret = ret + 1;      // 5
        _assert(v==&p1);
    end
end

switch l1 with
    case Nil() do
        ret = ret + 1;      // 6
        _assert(1);
    end
    case Cons(int head, List& tail) do
        _assert(0);
    end
end

switch l2 with
    case Nil() do
        _assert(0);
    end
    case Cons(int head1, List& tail1) do
        _assert(head1 == 1);
        ret = ret + 1;      // 7
        switch *tail1 with
            case Nil() do
                ret = ret + 1;      // 8
                _assert(1);
            end
            case Cons(int head2, List& tail2) do
                _assert(0);
            end
        end
        ret = ret + 1;      // 9
        _assert(1);
    end
end

switch l3 with
    case Nil() do
        _assert(0);
    end
    case Cons(int head1, List& tail1) do
        _assert(head1 == 1);
        ret = ret + 1;      // 10
        switch *tail1 with
            case Nil() do
                _assert(0);
            end
            case Cons(int head2, List& tail2) do
                _assert(head2 == 2);
                ret = ret + 2;      // 12
                switch *tail2 with
                    case Nil() do
                        _assert(1);
                        ret = ret + 1;      // 13
                    end
                    case Cons(int head3, List& tail3) do
                        _assert(0);
                    end
                end
                _assert(1);
                ret = ret + 1;      // 14
            end
        end
        _assert(1);
        ret = ret + 1;      // 15
    end
end

escape ret;
]],
    run = 15,
    todo = 'implement? trying only w/ named fields for now',
}
--]=]

-- TIMEMACHINE
local t = {
    [1] = [[
#define TM_QUEUE
]],
    [2] = [[
#define TM_QUEUE
#define TM_QUEUE_WCLOCK_REUSE
]],
    [3] = [[
#define TM_SNAP
]],
    [4] = [[
#define TM_SNAP
#define TM_QUEUE
]],
    [5] = [[
#define TM_SNAP
#define TM_QUEUE
#define TM_QUEUE_WCLOCK_REUSE
]],
    [6] = [[
#define TM_DIFF
]],
    [7] = [[
#define TM_SNAP
#define TM_DIFF
]],
}

for i=1, #t do
    local defs = t[i]

-- TODO: test OS_PAUSE/OS_RESUME

-- SEEK
Test { [[
native do
    int CEU_TIMEMACHINE_ON = 0;
end

class TM_App with
    var int v = 0;
do
    every 1s do
        this.v = this.v + 1;
    end
end
var TM_App tm_app;

input int DT;

]]..defs..[[

#define TM_INPUT_DT     DT
#define TM_QUEUE_N      1000000
#if defined(TM_QUEUE) || defined(TM_DIFF)
#define TM_SNAP_MS      2000
#endif
#define TM_SNAP_N       1000
#define TM_DIFF_N       1000000

pre native do
    ##define CEU_FPS 20
end

#include "tm/backend.ceu"

#ifdef TM_QUEUE
class IOTimeMachine with
    interface IIOTimeMachine;
do
end
var IOTimeMachine io;
#endif

var TimeMachine tm with
    this.app = &tm_app;
#ifdef TM_QUEUE
    this.io  = &io;
#endif
end;

par/or do
    await 3s/_;
    emit tm.go_on;
    await 1s/_;

    ///////////////////////////////

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_seek => 500;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_seek => 1000;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);

    emit tm.go_seek => 1500;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);

    emit tm.go_seek => 2000;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);

    emit tm.go_seek => 2500;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);

    emit tm.go_seek => 3000;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_seek => 2500;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);

    emit tm.go_seek => 2000;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);

    emit tm.go_seek => 1500;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);

    emit tm.go_seek => 1000;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);

    emit tm.go_seek => 500;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    escape 1;

with
    input int DT;
    async (tm) do
        loop do
            if not _CEU_TIMEMACHINE_ON then
                emit 50ms;
                emit DT => 50;
            end

            // TODO: forces this async to be slower
            input void SLOW;
            loop do
                if not tm.locked then
                    break;
                end
                emit SLOW;
            end
            emit 50ms/_;
        end
    end
end

escape tm_app.v;
]],
    timemachine = true,
    complete = (i>1),   -- runs i=1 for sure
    _ana = {
        acc = true,
    },
    run = 1,
}

-- Forward
Test { [[
native do
    int CEU_TIMEMACHINE_ON = 0;
end

class TM_App with
    var int v = 0;
do
    every 1s do
        this.v = this.v + 1;
    end
end
var TM_App tm_app;

input int DT;

]]..defs..[[

#define TM_INPUT_DT     DT
#define TM_QUEUE_N      1000000
#if defined(TM_QUEUE) || defined(TM_DIFF)
#define TM_SNAP_MS      2000
#endif
#define TM_SNAP_N       1000
#define TM_DIFF_N       1000000

pre native do
    ##define CEU_FPS 20
end

#include "tm/backend.ceu"

#ifdef TM_QUEUE
class IOTimeMachine with
    interface IIOTimeMachine;
do
end
var IOTimeMachine io;
#endif

var TimeMachine tm with
    this.app = &tm_app;
#ifdef TM_QUEUE
    this.io  = &io;
#endif
end;

par/or do
    await 3s/_;
    emit tm.go_on;
    await 1s/_;

    ///////////////////////////////

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    ///////////////////////////////

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_forward => 1;
    _assert(tm_app.v == 0);

    await 1ms/_;
    await 1000ms/_;
    _assert(tm_app.v == 1);
    await 1000ms/_;
    _assert(tm_app.v == 2);
    await 1000ms/_;
    _assert(tm_app.v == 3);

    ///////////////////////////////

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_forward => 1;
    _assert(tm_app.v == 0);

    await 1ms/_;
    loop i in [0 |> 20[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 1);
    loop i in [0 |> 20[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 2);
    loop i in [0 |> 20[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 3);

    ///////////////////////////////

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_forward => 2;
    _assert(tm_app.v == 0);

    await 1ms/_;
    loop i in [0 |> 10[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 1);
    loop i in [0 |> 10[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 2);
    loop i in [0 |> 10[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 3);

    ///////////////////////////////

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_forward => 5;
    _assert(tm_app.v == 0);

    await 1ms/_;
    await 200ms/_;
    _assert(tm_app.v == 1);
    await 200ms/_;
    _assert(tm_app.v == 2);
    await 200ms/_;
    _assert(tm_app.v == 3);

    ///////////////////////////////

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_forward => -2;
    _assert(tm_app.v == 0);

    await 1ms/_;
    await 2000ms/_;
    _assert(tm_app.v == 1);
    await 2000ms/_;
    _assert(tm_app.v == 2);
    await 2000ms/_;
    _assert(tm_app.v == 3);

    ///////////////////////////////

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    emit tm.go_forward => -5;
    _assert(tm_app.v == 0);

    await 1ms/_;
    loop i in [0 |> 100[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 1);
    loop i in [0 |> 100[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 2);
    loop i in [0 |> 100[ do
        await 50ms/_;
    end
    _assert(tm_app.v == 3);

with
    input int DT;
    async (tm) do
        loop do
            if not _CEU_TIMEMACHINE_ON then
                emit 50ms;
                emit DT => 50;
            end

            // TODO: forces this async to be slower
            input void SLOW;
            loop do
                if not tm.locked then
                    break;
                end
                emit SLOW;
            end
            emit 50ms/_;
        end
    end
end

escape tm_app.v;
]],
    timemachine = true,
    complete = (i>1),   -- runs i=1 for sure
    _ana = {
        acc = true,
    },
    run = 3,
}

-- Backward
Test { [[
native do
    int CEU_TIMEMACHINE_ON = 0;
end

class TM_App with
    var int v = 0;
do
    every 1s do
        this.v = this.v + 1;
    end
end
var TM_App tm_app;

input int DT;

]]..defs..[[

#define TM_INPUT_DT         DT
#define TM_QUEUE_N          1000000
#if defined(TM_QUEUE) || defined(TM_DIFF)
#define TM_SNAP_MS          2000
#endif
#define TM_SNAP_N           1000
#define TM_DIFF_N           1000000
#define TM_BACKWARD_TICK    30

pre native do
    ##define CEU_FPS 100
end

#include "tm/backend.ceu"

#ifdef TM_QUEUE
class IOTimeMachine with
    interface IIOTimeMachine;
do
end
var IOTimeMachine io;
#endif

var TimeMachine tm with
    this.app = &tm_app;
#ifdef TM_QUEUE
    this.io  = &io;
#endif
end;

par/or do
    await 3s/_;
    emit tm.go_on;
    await 1s/_;

    ///////////////////////////////

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_backward => 1;
    _assert(tm_app.v == 3);

    await 1000ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);
    await 1000ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);
    await 1000ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    ///////////////////////////////

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_backward => 1;
    _assert(tm_app.v == 3);

    loop i in [0 |> 20[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);
    loop i in [0 |> 20[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);
    loop i in [0 |> 20[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    ///////////////////////////////

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_backward => 2;
    _assert(tm_app.v == 3);

    loop i in [0 |> 10[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);
    loop i in [0 |> 10[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);
    loop i in [0 |> 10[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    ///////////////////////////////

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_backward => 5;
    _assert(tm_app.v == 3);

    await 200ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);
    await 200ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);
    await 200ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    ///////////////////////////////

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_backward => -2;
    _assert(tm_app.v == 3);

    await 2000ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);
    await 2000ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);
    await 2000ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    ///////////////////////////////

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_backward => -5;
    _assert(tm_app.v == 3);

    loop i in [0 |> 100[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 2);
    loop i in [0 |> 100[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);
    loop i in [0 |> 100[ do
        await 50ms/_;
    end
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    tm_app.v = tm_app.v + 1;
with
    input int DT;
    async (tm) do
        loop do
            if not _CEU_TIMEMACHINE_ON then
                emit 10ms;
                emit DT => 10;
            end

            // TODO: forces this async to be slower
            input void SLOW;
            loop do
                if not tm.locked then
                    break;
                end
                emit SLOW;
            end
            emit 10ms/_;
        end
    end
end

escape tm_app.v;
]],
    timemachine = true,
    complete = (i>1),   -- runs i=1 for sure
    _ana = {
        acc = true,
    },
    run = 1,
}

-- Forward / Backward
Test { [[
native do
    int CEU_TIMEMACHINE_ON = 0;
end

class TM_App with
    var int v = 0;
do
    every 1s do
        this.v = this.v + 1;
    end
end
var TM_App tm_app;

input int DT;

]]..defs..[[

#define TM_INPUT_DT     DT
#define TM_QUEUE_N      1000000
#if defined(TM_QUEUE) || defined(TM_DIFF)
#define TM_SNAP_MS      2000
#endif
#define TM_SNAP_N       1000
#define TM_DIFF_N       1000000

pre native do
    ##define CEU_FPS 100
end

#include "tm/backend.ceu"

#ifdef TM_QUEUE
class IOTimeMachine with
    interface IIOTimeMachine;
do
end
var IOTimeMachine io;
#endif

var TimeMachine tm with
    this.app = &tm_app;
#ifdef TM_QUEUE
    this.io  = &io;
#endif
end;

par/or do
    await 3s/_;
    _assert(tm_app.v == 3);
    emit tm.go_on;

    await 1s/_;
    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 0);

    await 1s/_;
    emit tm.go_forward => 2;
    _assert(tm_app.v == 0);

    await 1s400ms/_;
    _assert(tm_app.v == 2);

    emit tm.go_seek => tm.time_total;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 3);

    emit tm.go_backward => 2;
    _assert(tm_app.v == 3);

    await 1s1ms/_;
    TM_AWAIT_SEEK(tm);
    _assert(tm_app.v == 1);
with
    input int DT;
    async (tm) do
        loop do
            if not _CEU_TIMEMACHINE_ON then
                emit 10ms;
                emit DT => 10;
            end

            // TODO: forces this async to be slower
            input void SLOW;
            loop do
                if not tm.locked then
                    break;
                end
                emit SLOW;
            end
            emit 10ms/_;
        end
    end
end

escape tm_app.v;
]],
    timemachine = true,
    complete = (i>1),   -- runs i=1 for sure
    _ana = {
        acc = true,
    },
    run = 1,
}

Test { [[
native do
    int CEU_TIMEMACHINE_ON = 0;
end

input int&& KEY;
class TM_App with
    var int v = 0;
do
    par do
        every 1s do
            this.v = this.v + 1;
        end
    with
        every key in KEY do
            this.v = this.v * 2;
            this.v = this.v + *key;
        end
    end
end
var TM_App tm_app;

input int  DT;

]]..defs..[[

#define TM_INPUT_DT     DT
#define TM_QUEUE_N      1000000
#if defined(TM_QUEUE) || defined(TM_DIFF)
#define TM_SNAP_MS      2000
#endif
#define TM_SNAP_N       1000
#define TM_DIFF_N       1000000

pre native do
    ##define CEU_FPS 100
end

#include "tm/backend.ceu"

#ifdef TM_QUEUE
class IOTimeMachine with
    interface IIOTimeMachine;
do
    par do
        loop do
            // starts off
            watching this.go_on do
                every key in KEY do
                    do _queue_put(_CEU_IN_KEY,
                               sizeof(int), key as byte&&
#ifdef TM_SNAP
                                ,0
#endif
                              );
                        finalize with
                            nothing;
                        end;
                end
            end
            await this.go_off;
        end
    with
        every this.go_queue do
            var int v = *(_QU:buf);
            if _QU:evt == _CEU_IN_KEY then
                async(v) do
                    emit KEY => &&v;
                end
            else
                _assert(0);
            end
        end
    end
end
var IOTimeMachine io;
#endif

var TimeMachine tm with
    this.app = &tm_app;
#ifdef TM_QUEUE
    this.io  = &io;
#endif
end;

par/or do
    async do
        loop i in [0 |> 300[ do
            emit 10ms;
            emit DT => 10;
        end
        var int v = 1;
        emit KEY => &&v;
        loop i in [0 |> 300[ do
            emit 10ms;
            emit DT => 10;
        end
        v = 2;
        emit KEY => &&v;
        loop i in [0 |> 300[ do
            emit 10ms;
            emit DT => 10;
        end
    end
    _assert(tm_app.v == 25);

    emit tm.go_on;
    await 1s/_;

    emit tm.go_seek => 0;
    TM_AWAIT_SEEK(tm);

    emit tm.go_forward => 1;
    await 3s1ms/_;
    _assert(tm_app.v == 7);
    await 2s/_;
    _assert(tm_app.v == 9);
    await 1s/_;
    _assert(tm_app.v == 22);
    await 3s/_;
    _assert(tm_app.v == 25);
with
    input int DT;
    async (tm) do
        loop do
            // TODO: forces this async to be slower
            input void SLOW;
            loop do
                if not tm.locked then
                    break;
                end
                emit SLOW;
            end
            emit 10ms/_;
        end
    end
end

escape tm_app.v;
]],
    timemachine = true,
    complete = (i>1),   -- runs i=1 for sure
    _ana = {
        acc = true,
    },
    run = 25,
}

end

do return end

-------------------------------------------------------------------------------
-- BUGS & INCOMPLETNESS
-------------------------------------------------------------------------------

-- BUG: bad message, I want to say that you cannot copy vectors in a single stmt
Test { [[
class Test with
    code/instantaneous FillBuffer (vector[]&& u8 buf)=>void;
do
    code/instantaneous FillBuffer (vector[]&& u8 buf)=>void do
        vector[] u8 b = *buf;
        b = b .. [3];
    end
end

vector[10] u8 buffer;

var Test t;
t.fillBuffer(&&buffer);

escape buffer[0];
]],
    env = 'line 5 : types mismatch (`u8[]´ <= `u8[]´)',
}

-- BUG: doesn't check dimension of pointer to vector
Test { [[
code/instantaneous FillBuffer (vector[20]&& u8 buf)=>void do
    *buf = *buf .. [3];
end
vector[10] u8 buffer;
fillBuffer(&&buffer);
escape buffer[0];
]],
    env = 'line 5 : wrong argument #1 : types mismatch (`u8[]&&´ <= `u8[]&&´) : dimension mismatch',
}

--cbuffer "attr to greater scope"
Test { [[
    code/instantaneous/recursive Update_surface (void)=>void do
        var _CollisionMap&& colmap = _XXX_PURE(global:world!:get_colmap());

        this.me.colmap_serial = colmap:get_serial();

        this.me.canvas.lock();

        var u8&& cbuffer = (_XXX_PURE(this.me.canvas.get_data()) as u8&&);
]],
}

Test { [[
interface Global with
    var int i;
end
var int i = 1;

class Tx with
    code/instantaneous Get (void)=>int;
do
    code/instantaneous Get (void)=>int do
        escape global:i;
    end
end

var Tx t;

escape t.get();
]],
    run = 1,
}

Test { [[
class WorldObjFactory with
    var _PingusLevel&& plf;
do
    native do
        ##define std__vector_FileReader std::vector<FileReader>
    end
    loop i in [0|>this.plf:get_objects().size()[ do
        traverse _ in [] with
            var _FileReader&& reader = &&this.plf:get_objects().at(i);
        do
        end
    end
end
escape 1;
]],
    run = 1,
}

Test { [[
native/plain _rect;
pre native do
    typedef struct rect {
        int* x, y;
    } rect;
end
var int v = 10;
var _rect r = _rect(null);
r.x = &&v;      // BUG: finalize?
escape *(r.x);
]],
    run = 10,
}

Test { [[
data Dx with
    var int x;
end

data Ee with
    data Nothing;
or
    data Xx with
        var& Dx d;
    end
end

    var Dx d = Dx(1);
var Ee e = Ee.Xx(&d);
    e.Xx.d = &d;     // BUG: error?

escape e.Xx.d.x;
]],
    run = 1,
}

-- TODO: bug
Test { [[
data LLRB with
    data Nil;
or
    data Node with
        var LLRB left;
        var LLRB right;
    end
end

var& LLRB h;
h.Node.right = h.Node.left;

escape 1;
]],
    run = 1,
}

-- TODO: bug
Test { [[
data LLRB with
    data Nil;
or
    data Node with
        var LLRB left;
    end
end

pool[] LLRB llrb;
traverse e in llrb do
    e:Node.left = traverse e:Node.left;
end

escape 1;
]],
    run = 1,
}

-----------------------

Test { [[
class Tx with
    var int xxx2=0;
    code/instantaneous Fff (var int xxx3)=>void;
do
    code/instantaneous Fff (var int xxx3)=>void do
        var int xxx4 = xxx3;
        this.xxx2 = xxx4;
    end
    this.xxx2 = 1;
end

var int xxx1 = 10;
var Tx ttt;
ttt.fff(&xxx1);
escape ttt.xxx2;
]],
    run = 1,
}
Test { [[
class TimeDisplay with
    code/instantaneous Build (var& int vvv)=>TimeDisplay;
do
    var int x = 0;
    var& int vvv;

    code/instantaneous Build (var& int vvv)=>TimeDisplay do
        this.vvv = &vvv;
    end

    vvv = &x;
end
escape 1;
]],
    run = 1,
}

Test { [[
class TimeDisplay with
    code/instantaneous Build (var& int vvv)=>TimeDisplay;
do
    var& int vvv;

    code/instantaneous Build (var& int vvv)=>TimeDisplay do
        this.vvv = &vvv;
    end
end
escape 1;
]],
    run = 1,
}

--[=[
---

    var byte&&                  name    = null;

    code/instantaneous Name (var @hold byte&& name)=>Surface;

---

class Credits with
    var _Pathname&& filename;
do
    finalize with
        call {StatManager::instance()->set_bool}("credits-seen", true);
    end

    // read credit information from filename
    {
        static std::vector<std::string> credits;
        static int end_offset = -static_cast<float>(Display::get_height())/2 - 50; // screen height + grace time

        {
            std::ifstream in(THIS(CEU_Credits)->filename->get_sys_path());
            if (!in) {
                log_error("couldn't open %1%", THIS(CEU_Credits)->filename);

                std::ostringstream out;
                out << "couldn't open " << THIS(CEU_Credits)->filename;
]=]
-------------------------------------------------------------------------------

-- BUG: deveria ser outer.rect.
-- tenho que verificar essas atribuicoes this.x=this.x
        --var SpriteR _ = SpriteR.build_name(&this.rect,
                                           --"core/buttons/hbuttonbgb");

-- BUG: u8 vs int
Test { [[
native do
    ##define ceu_out_emit(a,b,c,d) __ceu_nothing_int(d,1)
end
output/input LINE [10] (var int max)=>int;
par/or do
    var u8 err;
    var u8? ret;
    (err, ret) = request LINE => 10;
with
end
escape 1;
]],
    run = 1,
}

Test { [[
emit/await/refs
class SDL with
    input:
        vector[] byte title;
        var int w,h;

    output:
        var& _SDL_Window   win;
        var& _SDL_Renderer ren;

    input/output:
        var int io;

    output/input:
        var int oi;
do
    var& _SDL_Window? win_;
        do win_ = &_SDL_CreateWindow("SDL 1", _SDL_WINDOWPOS_CENTERED,
                                           _SDL_WINDOWPOS_CENTERED,
                                           800, 480,
                                           _SDL_WINDOW_SHOWN);
    finalize with
        _SDL_DestroyWindow(&&win_!);
    end
    this.win = &win_!;

    _SDL_GetWindowSize(&&win, &&w, &&h);

    var& _SDL_Renderer? ren_;
        do ren_ = &_SDL_CreateRenderer(&&win, -1, 0);
    finalize with
        _SDL_DestroyRenderer(&&ren_!);
    end
    this.ren = &ren_!;

    await FOREVER;
end
var SDL _;
]],
    run = 1,
}

-- BUG: deallocates byte: ifc/body locs should not terminate
Test { [[
class Tx with
    output:
        vector&[] byte name;
do
    vector[] byte name_ = [].."oi";
    this.name = &name_;
    // bug: deallocates byte[]
end

var Tx t;
native/nohold _strlen;
escape _strlen((byte&&)&&t.name);
]],
    run = 2,
}

--
--no output vectors in interfaces

--
--every (x,_) in e do

--
--event in e      // class ifc
--if e then ...   // nested blk in body
--end
--_printf(e);

--
-- bug: arity mismatch on constructor/creation
Test { [[
class Tx with
    var& void p;
    code/instantaneous Build (var& void p)=>Tx;
do
    code/instantaneous Build (var& void p)=>Tx do
        this.p = &p;
    end
    escape *(&&this.p as int&&);
end

var int v = 10;
var int ret = do Tx.build(&v);
escape ret;
]],
    run = 10,
}

Test { [[
data Dx with
    vector&[] byte str;
end
vector[] byte s = [].. "oi";
var Dx d = Dx(s);    // BUG: nao detecta erro de tipo
escape $d.str;
]],
    run = 2,
}

-- bug: force nominal type system
Test { [[
interface I with
end

class V with
do
    pool[1] I is;
end

escape 1;
]],
    run = 1,
}

-- async dentro de pause
-- async thread spawn falhou, e ai?

Test { [[
native _u8;
data Test with
  var _u8[10] vvv;
end
native _V;
var Test t = Test([_V]);    // should not accept [_V] here
t.vvv[9] = 10;
escape t.vvv[9];
]],
    run = 10,
}

-- BUG-EVERY-SPAWN
-- t1 creates t2, which already reacts to e
Test { [[
native do
    int V = 0;
end

class Gx with
    event void e;
do
    await FOREVER;
end

interface I with
    var& Gx g;
end

pool[] I is;

class Tx with
    var& Gx g;
    pool&[] I is;
do
    every g.e do
native _V;
        _V = _V + 1;
        spawn Tx in is with
            this.g = &outer.g;
            this.is = &outer.is;
        end;
    end
end

var Gx g;
spawn Tx in is with
    this.g = &g;
    this.is = &is;
end;
emit g.e;

escape _V;
]],
    run = 1,
}

-- BUG: must enforce alias
Test { [[
data Ball with
    var int x;
end

data Leaf with
    data Nothing;
or
    data Tween with
        var& Ball ball;
    end
end

class LeafHandler with
    var& Leaf leaf;
do
    var& Ball ball = &leaf.Tween.ball;
    escape ball.x;
end

var Ball ball = Ball(10);
var Leaf leaf = Leaf.Tween(ball);   // must be alias

var int x = do LeafHandler with
                this.leaf = &outer.leaf;
            end;

escape x;
]],
    todo = 'bug',
    run = 1,
}
---------------------------------------------------
-- BUG: should be type error, Tx&& <= Tx[]
Test { [[
data Tree;
    data Nil;
or
    data Node with
        var int   v;
        var Tree  left;
        var Tree  right;
    end
end

pool[3] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

class Sum with
    var int&& v;
do
    await FOREVER;
end

class Body with
    pool&[]  Body bodies;
    var   Tree&&   n;
    var&   Sum    sum;
do
    watching *n do
        if (*n is Node) then
            *this.sum.v = *this.sum.v + (*n as Node).v;
            spawn Body in this.bodies with
                this.bodies = &bodies;
                this.n      = &&(*n as Node).left;
                this.sum    = &sum;
            end;
        end
    end
end

var int v = 0;
var Sum sum with
    this.v = &&v;
end;

pool[7] Body bodies;
do Body with
    this.bodies = &bodies;
    this.n      = &&tree;
    this.sum    = &sum;
end;

escape v;
]],
    todo = 'bug',
    fin = 'line 29 : unsafe access to pointer "v" across `class´ (tests.lua : 22)',
}
Test { [[
data List;
    data Nil;
    data Cons is List with
        var int  head;
        var List tail;
    end
end

var List l;
escape 1;
]],
    todo = 'List is recursive',
    run = 1,
}

Test { [[
class Tx with
    vector[] byte xxx;
do
    await FOREVER;
end

var Tx t with
    this.xxx = [].."oioi";
end;

escape $t.xxx;
]],
    run = 4,
}

-- TODO: vectors in the class interface
Test { [[
native/pure _strlen;
class Tx with
    vector[] byte str;
do
    escape _strlen(&&this.str);
end
var int n = do Tx with
                this.str = [] .. "1234";
            end;
escape n;
]],
    run = 1,
}

-- BUG: same id
-- BUG: unassigned var (should be int?)
Test { [[
var int a = 10;
do
    var int a = a;
    escape a;
end
]],
    wrn = true,
    run = 10,
}

Test { [[
data Stmt;
    data Nil;
or
    data Seq is Stmt with
        var Stmt s1;
    end
end

pool[] Stmt stmts = new Stmt.Nil();

var int v1 = 10;

var int ret =
    traverse stmt in stmts with
        var int v1 = v1+1;
    do
        escape v1;
    end;

escape ret;
]],
    wrn = true,
    run = 11,
}

-- BUG: across
-- BUG: unassigned pointer (should be int*?)
Test { [[
var int* x;
await 1s;
escape *x;
]],
    todo = true,
    run = 1,
}

Test { [[
var& void v;
escape 1;
]],
    run = 1,
    -- TODO: should fail as below
    --env = 'line 1 : cannot instantiate type "void"',
}

-- TODO: bug: what if the "o" expression contains other pointers?
-- (below: pi)
Test { [[
class Tx with
do
end

vector[10] Tx ts;
var int   i = 0;
var int* pi = &i;
await ts[*pi];
escape 1;
]],
    fin = 'line 8 : pointer access across `await´',
}
-- should disallow passing pointers through internal events
Test { [[
input void OS_START;
event int* e;
var int ret = 0;
par/or do
    do
        var int x = 2;
        par/or do
            await OS_START;
            emit e => &x;
        with
            await e;
        end
    end
    do
        var int x = 1;
        await 1s;
        ret = x;
    end
with
    var int* v = await e;
    ret = *v;
end
escape ret;
]],
    run = 2,
}

-- XXX: Tx-vs-Opt
Test { [[
class Tx with
do
end
var& Tx*? t;
    do t = _malloc(10 * sizeof(Tx**));
finalize with
    nothing;
end
native/nohold _free;
do finalize with
    _free(t!);
end
escape 10;
]],
    run = 10;
}

Test { [[
input void A, B, Z;
event void a;
var int ret = 1;
native _t;
var _t* a;
native _f;
par/or do
    do _f(a);               // 8
        finalize with
            ret = 1;    // DET: nested blks
        end;
with
    var _t* b;
    do _f(b);               // 14
        finalize with
            ret = 2;    // DET: nested blocks
        end;
end
escape ret;
]],
    _ana = {
        acc = 2,
    },
    run = false,
}

Test { [[
input void OS_START;
event int a, x, y;
var int ret = 0;
par do
    par/or do
        await y;
        escape 1;   // 12
    with
        await x;
        escape 2;   // 15
    end;
with
    await OS_START;
    emit x => 1;       // in seq
    emit y => 1;       // in seq
end
]],
    _ana = {
        acc = 0,
    },
    run = 2;
}

Test { [[
input void OS_START;
native _V;
native do
    int V = 1;
end
class Tx with
do
    par/or do
        await OS_START;
    with
        await OS_START;    // valgrind error
    end
    _V = 10;
end
do
    spawn Tx;
    await 1s;
end
escape _V;
]],
    run = { ['~>1s']=10 },
}

Test { [[
input int  A;
input void Z;
event int a;
var int ret = 0;
par/or do
    loop do
        var int v = await A;
        emit a => v;
    end
with
    pause/if a do
        ret = await 9us;
    end
end
escape ret;
]],
    run = {
        ['~>1us;0~>A;~>1us;0~>A;~>19us'] = 12,
        ['~>1us;1~>A;~>1s;0~>A;~>19us'] = 11,
        --['~>1us;1~>A;~>5us;0~>A;~>5us;1~>A;~>5us;0~>A;~>9us'] = 6,
-- BUG: set_min nao eh chamado apos o pause
    },
}

Test { [[
input void A,B;

interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
    await inc;
    this.v = v + 1;
end

var int ret = 0;
do
    par/or do
        await B;
    with
        var int i=1;
        every 1s do
            spawn Tx with
                this.v = i;
                i = i + 1;
            end;
        end
    with
        every 1s do
            loop i in I* do
                emit i:inc;         // mata o org enquanto o percorre iterador
                ret = ret + i:v;
            end
        end
    end
end
escape ret;
]],
-- BUG: erro de valgrind
    run = { ['~>3s;~>B'] = 11 },
}

-- BUG: should be: field must be assigned
Test { [[
var int v = 10;
var& int i;

par do
    await 1s;
    i = v;
with
    escape i;
end
]],
    run = 99,
}

error 'testar pause/if org.e'
error 'testar spawn/spawn que se mata'

--do return end

-- ok: under tests but supposed to work

--ERROR: #ps
Test { [[
input (int,int,int) EVT;
var int a,b;
(a,b) = await EVT;
escape 1;
]],
    run = 1,
}
-- ERROR: defs.h before host code
-- makes sense: how an external component would know about a
-- type defined in Ceu?
Test { [[
pre native do
    typedef int t;
end
native _t ;
input (_t,int) EVT;
escape 1;
]],
    run = 1,
}

-- ERROR: parse (typecast)
Test { [[
if ( _transaction ) then
    _coap_send_transaction(_transaction);
end
]],
    run = 1,
}

Test { [[
input void OS_START;
event (int,void*) ptr;
var int* p;
var int i;
par/or do
    (i,p) = await ptr;
with
    do
        var int b = 1;
        await OS_START;
        emit ptr => (1, &b);
    end
end
escape 1;
]],
    run = 1,
    -- e depois outro exemplo com fin apropriado
    -- BUG: precisa transformar emit x=>1 em p=1;emit x
}

Test { [[
native do
    int V = 0;
end

class Tx with
do
    _V = 10;
    do finalize with
        _V = 100;   // TODO: deveria executar qd "var Tx t" sai de escopo
    end
end

var Tx t;
_assert(_V == 10);
escape _V;
]],
    run = 100,
}

Test { [[
code/instantaneous Fx () => void;
escape 1;
]],
    run = 1,
}

-- TODO: fails on valgrind, fails on OS
-- put back to XXXX
Test { [[
native _V;
input void A, B, OS_START;
native do
    int V = 0;
end
class Tx with
    event void e, ok;
    var int v;
do
    do finalize with
        _V = _V + 1;        // * writes before
    end
    v = 1;
    await A;
    v = v + 3;
    emit e;
    emit ok;
end
await OS_START;
var int ret;
do
    var Tx t;
    par/or do
        do                  // 24
            do finalize with
                _V = _V*10;
            end
            await t.ok;
        end
    with
        await t.e;          // 31
        t.v = t.v * 3;
    with
        await B;
        t.v = t.v * 5;
    end
    ret = t.v;
end
escape ret + _V;        // * reads after
]],
    _ana = {
        abrt = 1,        -- false positive
    },
    run = {
        ['~>B'] = 6,
        ['~>A'] = 13,
    }
}

-- TODO_TYPECAST (search and replace)
Test { [[
class Tx with
do
end
// TODO: "typecast" esconde "call", finalization nao acha que eh call
var Tx** t := (Tx**)_malloc(10 * sizeof(Tx**));
native/nohold _free;
do finalize with
    _free(t);
end
escape 10;
]],
    run = 10;
}

-- varlist to iter
Test { [[
interface I with
    var int v;
end
class Tx with
    interface I;
do
end
pool[1] Tx ts;
var Tx a with
    a.v = 15;
end
var int ret = 0;
ret = ret + spawn Tx[ts] with
                this.v = 10;
            end;
ret = ret + spawn Tx[ts];
loop i in (I*)(ts in a) do
    ret = ret + i:v;
end
escape 26;
]],
    run = 1,
}

Test { [[
class Tx with
    var void* ptr = null;
do
end
var Tx* ui;
do
    pool[] Tx ts;
    var void* p = null;
    ui = spawn Tx in ts with // ui > ts (should require fin)
        this.ptr = p;
    end;
end
escape 10;
]],
    run = 1,
}

--[=[
-- POSSIBLE PROBLEMS FOR UNITIALIZED VAR

Test { [[
var int r;
var int* pr = &r;
async(pr) do
    var int i = 100;
    *pr = i;
end;
escape r;
]],
    run=100
}

Test { [[
var int a;
par/or do
    await 1s;
    a = 1;
with
end
escape a;
]],
    run = 10,
}

Test { [[
vector[2] int v ;
_f(v)
escape v == &v[0] ;
]],
    run = 1,
}

Test { [[
native/nohold _strncpy(), _printf(), _strlen();
native _char = 1;
var _char[10] str;
_strncpy(str, "123", 4);
_printf("END: %d %s\n", (int)_strlen(str), str);
escape 0;
]],
    run = '3 123'
}

Test { [[
var int a;
a = do
    var int b;
end;
]],

Test { [[
class Tx with
    var int* a1;
do
    var int* a2 = a1;
end
escape 10;
]],
    run = 10,
}

}

]=]

-------------------------------------------------------------------------------

-- TODO: should require finalization
Test { [[
class Tx with
native _int;
    var _int to;
do
end

var _int to = 1;

var Tx move with
    this.to = to;  // TODO: := ??
end;

escape move.to;
]],
    run = 1,
}

-- TODO: I[100]
Test { [[
interface I with
    var int v;
end

class Tx with
    interface I;
do
    await FOREVER;
end

pool[100] I is;

var int ret = 0;

spawn Tx with
    this.v = 1;
end;

spawn Tx in is with
    this.v = 3;
end;

loop i in is do
    ret = ret + i:v;
end

escape ret;
]],
    run = 3,
}

-- TODO: spawn wrong type
Test { [[
interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
    await FOREVER;
end
pool[] I is;

class U with
    var int z;
    var int v;
do
    await FOREVER;
end

var int ret = 0;
do
    spawn Tx with
        this.v = 1;
    end;
    spawn U in is with
        this.v = 2;
    end;
    spawn Tx in is with
        this.v = 3;
    end;

    loop i in is do
        ret = ret + i:v;
    end
end
escape ret;
]],
    run = 5,
}

-- U[10] vs U[] mismatch
Test { [[
class U with do end;

interface I with
    pool[10] U us;
end

interface Global with
    interface I;
end
pool[] U  us;

class Tx with
    pool[10] U us;
    interface I;
do
    spawn U in global:us;
end

spawn U in us;
spawn U in global:us;

pool[1] U us1;
spawn U in us1;

var Tx t;
spawn U in t.us;

var I* i = &t;
spawn U in i:us;

escape 1;
]],
    wrn = true,
    run = 1,
}

-- TODO: invalid pointer access
Test { [[
var int* ptr = null;
loop i in [0 |> 100[ do
    await 1s;
    var int* p;
    if (ptr != null) then
        p = ptr;
    end
    ptr = p;
end
escape 10;
]],
    --loop = true,
    fin = 'line 5 : invalid pointer "ptr"',
}

-- TODO: t.v // Tx.v
Test { [[
class Tx with
    var int v;
do
    v = 1;
end
var Tx t;
t.v = 10;
escape t.v;
]],
    run = 10,
}

-- global vs assert??
Test { [[
interface Global with
    event void e;
end
event void e;
par/or do
    emit global:e;
with
    _assert(0);
end
escape 1;
]],
    run = 1,
}

-- this vs _iter??
Test { [[
interface I with
    var int v;
end

class Tx with
    interface I;
do
    this.v = 1;
end
pool[] Tx ts;

par/or do
    spawn Tx in ts with
    end;
with
    loop i in ts do
    end
end

escape 1;
]],
    run = 1,
}

-- TODO: spawn vs watching impossible
Test { [[
class Tx with
do
end

par/and do
    pool[] Tx ts;
    var Tx* t = spawn Tx in ts with
    end;
with
    var Tx* p;
    watching p do
    end
end

escape 1;
]],
    run = 1,
}

-- TODO: explicit interface implementations only
Test { [[
interface I with
    var int v;
end

class Tx with
    var int u,v,x;
do
end

class U with
    var int v;
do
end

class V with
    var int v;
do
    pool[10] I is;
    spawn Tx in is;
    spawn U in is;
end

pool[10] I is;

spawn Tx in is;
spawn U in is;
spawn V in is;

escape sizeof(CEU_T) > sizeof(CEU_U);
]],
    run = 1,
}

-- TODO: not "awake once" for await-until
Test { [[
input void OS_START;
event int v;
par do
    var int x;
    x = await v until x == 10;
    escape 10;
with
    await OS_START;
    emit v => 0;
    emit v => 1;
    emit v => 10;
    await FOREVER;
end
]],
    run = 10;
}

-------------------------------------------------------------------------------
Test { [[
input void    START,   STOP;
input _pkt_t* RECEIVE, SENDACK;

native/nohold _memcpy(), _send_dequeue(), _pkt_setRoute(), _pkt_setContents(), 
_receive();

class Forwarder with
   var _pkt_t pkt;
   event void ok;
do
   loop do
      var bool enq;
      do enq = _send_enqueue(&pkt);
            finalize with
               _send_dequeue(&pkt);
            end;
      if not enq then
         await (_rand()%100)ms;
         continue;
      end
      var _pkt_t* done;
      done = await SENDACK
             until (done == &pkt);
      break;
   end
   emit this.ok;
end

class Client with
do
   loop seq do
      par/and do
         await 1min;
      with
         do Forwarder with
            _pkt_setRoute(&this.pkt, seq);
            _pkt_setContents(&this.pkt, seq);
         end;
      end
   end
end

loop do
   await START;
   par/or do
      await STOP;
   with
      pool[10] Forwarder forwarders;
      vector[10]  Client    clients;

      var _pkt_t* pkt;
      every pkt in RECEIVE do
         if pkt:left == 0 then
            _receive(pkt);
         else
            pkt:left = pkt:left - 1;
            spawn Forwarder with
               _memcpy(&this.pkt, pkt, pkt:len);
            end;
         end
      end
   end
end
]],
    run = 0,
}

Test { [[
input int* A;
par/or do
    var int* snd = await A;
    *snd = *snd;
    await FOREVER;
with
    var int* snd =
        await A
            until *snd == 1;
    escape *snd;
with
    async do
        var int i = 2;
        emit A => &i;
        i = 1;
        emit A => &i;
    end
end
escape 0;
]],
    _ana = {
        acc = 4,
    },
    run = 1;
}

Test { [[
class Rect with
do
    await FOREVER;
end

if false then
    interface Bird with end
    var Bird* ptr = null;
    watching ptr do end
else
    pool[257] Rect rs;
    loop i in [0 |> 257[ do
        spawn Rect in rs;
    end
end

escape 10;
]],
    run = 10,
}

Test { [[
class Tx with do end;
pool[] Tx ts;
loop t in ts do
    await 1s;
end
escape 1;
]],
    props = 'line 4 : `every´ cannot contain `await´',
}

Test { [[
interface I with
end
class Tx with
    interface I;
do end
do
    pool[] Tx ts;
    loop i in ts do
        await 1s;
    end
end
escape 1;
]],
    props = 'line 9 : `every´ cannot contain `await´',
}

Test { [[
interface I with
    var int v;
end

var I* i=null;

par/or do
    await 1s;
with
    watching i do
        await 1s;
        var int v = i:v;
    end
end

escape 1;
]],
    run = 1,
}

--BUG de "&" para org across await

-- TODO: (_XXX) eh um cast => msg melhor!
Test { [[
if (_XXX) then
end
]],
    run = 1,
}

-- PROCURAR XXX e recolocar tudo ate o ok la

Test { [[
input B  (var int a)=>int do
    escape a + 1;
end
var int ret = call B=>1;
escape ret;
]],
    run = 2,
}

Test { [[
input WRITE  (var int c)=>int do
    escape c + 1;
end
var byte b = 1;
var int ret = call WRITE => b;
escape ret;
]],
    run = 2,
}

Test { [[
input B  (var int a, var  int b)=>int do
    escape a + b;
end
var int ret = call B=>(1,2);
escape ret;
]],
    run = 3,
}

Test { [[
pre native do
    typedef int lua_State;
    void lua_pushnil (lua_State* l) {}
end

input PUSHNIL  (var _lua_State* l)=>void do
    _lua_pushnil(l);
end
escape 1;
]],
    run = 1,
}

Test { [[
input DRAW_STRING  (var byte* str, var int len, var int x, var  int y)=>int do
    escape x + y + len;
end

var int ret = call DRAW_STRING => ("Welcome to Ceu/OS!\n", 20, 100, 100);

escape ret;
]],
    run = 220,
}

Test { [[
input MALLOC  (void)=>void*;
var void* ptr = (call MALLOC);
]],
    fin = 'line 2 : destination pointer must be declared with the `[]´ buffer modifier',
}

Test { [[
input MALLOC  (void)=>void*;
vector[] void ptr = (call MALLOC);
]],
    fin = 'line 2 : attribution requires `finalize´',
}

Test { [[
input MALLOC  (void)=>void*;
vector[] void ptr;
    do ptr = (call MALLOC);
finalize with
end
escape 1;
]],
    code = 'line 1 : missing function body',
}

Test { [[
input MALLOC  (var int, var int)=>void*;
vector[] void ptr;
    do ptr = (call MALLOC=>(1,1));
finalize with
end
escape 1;
]],
    code = 'line 1 : missing function body',
}

Test { [[
input MALLOC  (var int, var int)=>int;
var int v;
    do v = (call MALLOC=>(1,1));
finalize with
end
escape 1;
]],
    fin = 'line 4 : attribution does not require `finalize´',
}

Test { [[
input MALLOC  (var int a, var int b, var  void* ptr)=>void* do
    if a+b == 11 then
        escape ptr;
    else
        escape null;
    end
end

var int i;
vector[] void ptr;
    do ptr = (call MALLOC=>(10,1, &i));
finalize with
end
escape ptr==&i;
]],
    run = 1,
}
Test { [[
input MALLOC  (var int a, var int b, var  void* ptr)=>void* do
    if a+b == 11 then
        escape ptr;
    else
        escape null;
    end
end

var int i;
vector[] void ptr;
    do ptr = (call MALLOC=>(1,1, &i));
finalize with
end
escape ptr==null;
]],
    run = 1,
}

Test { [[
input MALLOC  (void)=>void*;
native _f;
do
    var void* a;
        do a = (call MALLOC);
    finalize with
        do await FOREVER; end;
    end
end
]],
    fin = 'line 6 : destination pointer must be declared with the `[]´ buffer modifier',
}

Test { [[
input B  (var void* v)=>void do
    _V = v;
end
escape 1;
]],
    fin = 'line 2 : attribution to pointer with greater scope',
}

Test { [[
input B  (var void* v)=>void do
    _V := v;
end
escape 1;
]],
    fin = 'line 2 : parameter must be `hold´',
}

Test { [[
native do
    void* V;
end
input B  (var/hold void* v)=>void do
    _V := v;
end
escape 1;
]],
    run = 1,
}

Test { [[
input B  (var byte* buf)=>void do
end;
var byte* buf;
call B => (buf);
escape 1;
]],
    run = 1,
}

Test { [[
input B  (var byte* buf, var  int i)=>void do
end;
var byte* buf;
call B => (buf, 1);
escape 1;
]],
    run = 1,
}

Test { [[
input B  (void)=>void do
end;
var byte* buf;
call B;
escape 1;
]],
    run = 1,
}

Test { [[
input B  (var byte* buf)=>void do
end;
var byte* buf;
call B => buf;
escape 1;
]],
    run = 1,
}

Test { [[
input B  (var/hold byte* buf)=>void do
end;
var byte* buf;
call B => buf;
escape 1;
]],
    fin = 'line 2 : call requires `finalize´',
}

Test { [[
vector[255] byte buf;
_enqueue(buf);
escape 1;
]],
    fin = 'line 2 : call requires `finalize´',
}

Test { [[
native _f;
do
    var int* p1 = null;
    do
        var int* p2 = null;
        _f(p1, p2);
    end
end
escape 1;
]],
    wrn = true,
    fin = 'line 6 : call requires `finalize´',
    -- multiple scopes
}

Test { [[
native _f;
native _v;
native do
    int v = 1;
    int f (int v) {
        escape v + 1;
    }
end
escape _f(_v);
]],
    --fin = 'line 3 : call requires `finalize´',
    run = 2,
    --fin = 'line 9 : attribution requires `finalize´',
}
Test { [[
native/pure _f;
native _v;
native do
    int v = 1;
    int f (int v) {
        escape v + 1;
    }
end
escape _f(_v);
]],
    --fin = 'line 3 : call requires `finalize´',
    run = 2,
}


Test { [[
native/pure _f;
native do
    int* f (int a) {
        escape NULL;
    }
end
var int* v = _f(0);
escape v == null;
]],
    run = 1,
}

Test { [[
native/pure _f;
native do
    int V = 10;
    int f (int v) {
        escape v;
    }
end
native/const _V;
escape _f(_V);
]],
    run = 10;
}

Test { [[
native _f;
native do
    int f (int* v) {
        escape 1;
    }
end
var int v;
escape _f(&v) == 1;
]],
    fin = 'line 8 : call requires `finalize´',
}

Test { [[
native/nohold _f;
native do
    int f (int* v) {
        escape 1;
    }
end
var int v;
escape _f(&v) == 1;
]],
    run = 1,
}

Test { [[
native _V;
native/nohold _f;
native do
    int V=1;
    int f (int* v) {
        escape 1;
    }
end
var int v;
escape _f(&v) == _V;
]],
    run = 1,
}

Test { [[
input B  (var int* p1, var  int* p2)=>void;
do
    var int* p1 = null;
    do
        var int* p2 = null;
        call B => (p1, p2);
    end
end
escape 1;
]],
    fin = 'line 6 : invalid call (multiple scopes)',
}

-- TODO: finalize not required
Test { [[
native do
    #define ceu_out_call_VVV(x) x
end

output VVV  (var int n)=>int;
var int v;
    do v = (call VVV => 10);
finalize with
    nothing;
end
escape v;
]],
    run = 10,
}

-- TODO: finalize required
Test { [[
native do
    #define ceu_out_call_MALLOC(x) NULL
end

output MALLOC  (var int n)=>void*;
var byte* buf;
buf = (call MALLOC => 10);
escape 1;
]],
    run = 1,
}

-- TODO: finalize required
Test { [[
native do
    #define ceu_out_call_SEND(x) 0
end

output SEND  (var byte* buf)=>void;
vector[255] byte buf;
call SEND => buf;
escape 1;
]],
    run = 1,
}

-- TODO: finalize required
Test { [[
pre native do
    typedef struct {
        int a,b,c;
    } Fx;
end
native do
    Fx* f;
    #define ceu_out_call_OPEN(x) f
end
output OPEN  (var byte* path, var  byte* mode)=>_Fx*;

// Default device
var _Fx[] f;
    f = (call OPEN => ("/boot/rpi-boot.cfg", "r"));
escape 1;
]],
    run = 1,
}

Test { [[
output OPEN  (var byte* path, var  byte* mode)=>_Fx*;
output CLOSE  (var _Fx* f)=>int;
output SIZE  (var _Fx* f)=>int;
output READ  (var void* ptr, var int size, var int nmemb, var  _Fx* f)=>int;

// Default device
var _Fx[] f;
    do f = (call OPEN => ("/boot/rpi-boot.cfg", "r"));
finalize with
    call CLOSE => f;
end

if f == null then
    await FOREVER;
end

var int flen = (call SIZE => f);
//byte *buf = (byte *)malloc(flen+1);
vector[255] byte buf;
buf[flen] = 0;
call READ => (buf, 1, flen, f);

#define GPFSEL1 ((uint*)0x20200004)
#define GPSET0  ((uint*)0x2020001C)
#define GPCLR0  ((uint*)0x20200028)
var uint ra;
ra = *GPFSEL1;
ra = ra & ~(7<<18);
ra = ra | 1<<18;
*GPFSEL1 = ra;

var byte* orig = "multiboot";

loop do
    loop i in [0 |> 9[ do
        if buf[i] != orig[i] then
            await FOREVER;
        end
        *GPCLR0 = 1<<16;
        await 1s;
        *GPSET0 = 1<<16;
        await 1s;
    end
end
]],
    run = 1,
    --todo = 'finalize is lost!',
}

Test { [[
vector[10] int vec1;

class Tx with
    var& int* vec2;
do
    this.vec2[0] = 10;
end

vec1[0] = 0;

var Tx t with
    this.vec2 = outer.vec1;
end;

escape vec1[0];
]],
    run = 10,
}

-------------------------------------------------------------------------------

-- BUG: new inside constructor (requires stack manipulation?)
Test { [[
data Command;
    data Nothing;
or
    data Sequence is Command with
        var Command* one;
        var Command* two;
    end
end

class TCommand with
    pool[] Command cs;
do
end

var TCommand cmds with
    this.cs = new Nothing();
end;

escape 1;
]],
    run = 1,
}

-- TODO: BUG: type of bg_clr changes
--          should yield error
--          because it stops implementing UI
Test { [[
interface UI with
    var&   int?   bg_clr;
end
class UIGridItem with
   var UI* ui;
do
    watching ui do
        await FOREVER;
    end
end
class UIGrid with
    interface UI;
    var&   int?    bg_clr = nil;
    pool[] UIGridItem uis;
do
end

var UIGrid g1;
var UIGrid g2;
spawn UIGridItem in g1.uis with
    this.ui = &g2;
end;

escape 1;
]],
    run = 1,
}

Test { [[
interface UI with
end
class UIGridItem with
   var UI* ui;
do
    watching ui do
        await FOREVER;
    end
end
class UIGrid with
    interface UI;
    pool[] UIGridItem uis;
do
end

do
    var UIGrid g1;
    var UIGrid g2;
    spawn UIGridItem in g1.uis with
        this.ui = &g2;
    end;
end

escape 1;
]],
    run = 1,
}

Test { [[
pre native do
    typedef struct {
        int v;
    } tp;
end
class Tx with
    var& _tp? i = nil;
do
end
var Tx t;
escape t.i!==nil;
]],
    run = 1,
}

Test { [[
pre native do
    typedef struct {
        int v;
    } tp;
    tp V = { 10 };
end
class Tx with
    var& _tp? i = nil;
do
end
var Tx t with
    this.i = &_V;
end;
escape t.i!.v;
]],
    run = 10,
}

Test { [[
_assert(0);
escape 1;
]],
    asr = true,
}

-- BUG: do Tx quando ok acontece na mesma reacao
Test { [[
class Body with
    pool&[]  Body bodies;
    var&   int    sum;
    event int     ok;
do
    do finalize with end;

    var Body* nested =
        spawn Body in bodies with
            this.bodies = bodies;
            this.sum    = sum;
        end;
    if nested != null then
        watching nested do
            await nested:ok;
        end
        sum = sum + 1;
    end
    emit this.ok => 1;
end

pool[2] Body bodies;
var  int     sum = 0;

    do finalize with end;

do Body with
    this.bodies = bodies;
    this.sum    = sum;
end;

escape sum;
]],
    wrn = 'line 7 : unbounded recursive spawn',
    run = 6,
}

-- BUG: do Tx quando ok acontece na mesma reacao
Test { [[
data Tree;
    data Nil;
with
    data Node with
        var int   v;
        var Tree* left;
        var Tree* right;
    end
end

pool[3] Tree tree;
tree = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

class Body with
    pool&[]  Body bodies;
    var   Tree*   n;
    var&   int    sum;
    event int     ok;
do
    //watching n do
        var int i = this.sum;
        if (*n is Node) then
            var Body* left =
                spawn Body in this.bodies with
                    this.bodies = bodies;
                    this.n      = (*n as Node).left;
                    this.sum    = sum;
                end;
            //watching left do
                await left:ok;
            //end

            this.sum = this.sum + i + (*n as Node).v;

            var Body* right =
                spawn Body in this.bodies with
                    this.bodies = bodies;
                    this.n      = (*n as Node).right;
                    this.sum    = sum;
                end;
            //watching right do
                await right:ok;
            //end

            //do/spawn Body in this.bodies with
                //this.n = (*n as Node).left;
            //end;
        end
    //end
    emit this.ok => 1;
end

var int sum = 0;

pool[7] Body bodies;
do Body with
    this.bodies = bodies;
    this.n      = &&tree;
    this.sum    = sum;
end;

escape sum;

/*
var int sum = 0;
loop n in &&tree do
    var int i = sum;
    if (*n is Node) then
        traverse &(*n as Node).left;
        sum = i + (*n as Node).v;
        traverse &(*n as Node).right;
    end
end
escape sum;
*/
]],
    wrn = 'line 26 : unbounded recursive spawn',
    run = 999,
}

-- BUG: loop between declaration and watching
Test { [[
class Tx with
    event void e;
do
    await FOREVER;
end

pool[] Tx ts;

var Tx*? t = spawn Tx in ts;

loop do
    watching *t! do
        kill *t!;
    end
    await 1s;
    if false then
        break;
    end
end

escape 1;
]],
    run = { ['~>1s']=10 },
}

Test { [[
class Tx with
    event void e;
do
    await e;
end

pool[] Tx ts;

var int ret = 1;

spawn Tx in ts;
spawn Tx in ts;
async do end;

loop t1 in ts do
    //watching *t1 do
        loop t2 in ts do
            watching *t1 do
                ret = ret + 1;
                emit t1:e;
                ret = ret + 1;
            end
        end
    //end
end

escape ret;
]],
    run = 3,
}

-- TODO: precisa do watching
Test { [[
data Tree;
    data Nil;
with
    data Node with
        var int   v;
        var Tree* left;
        var Tree* right;
    end
end

pool[3] Tree t;
t = new Node(1,
            Node(2, Nil(), Nil()),
            Node(3, Nil(), Nil()));

var int sum = 0;

par/or do
    loop i in t do
        if (*i is Node) then
            traverse &(*i as Node).left;
            await 1s;
            sum = sum + (*i as Node).v;
            traverse &(*i as Node).right;
            await 1s;
        end
    end
with
    // 1->2->l
    _assert(sum == 0);
    await 1s;
    _assert(sum == 2);
    // 1->*->d
    await 1s;
    await 1s;
    _assert(sum == 3);
    // *->3->l
    await 1s;
    _assert(sum == 6);
    // *->*->r
    await 1s;
    await 1s;
    sum = 0;
end

escape sum;
]],
    _ana = { acc=true },
    run = { ['~>10s']=6 },
}

-- BUG: parser cannot handle 65k lines
local str = {}
for i=1, 65536 do
    str[#str+1] = [[
class Class]]..i..[[ with
    interface I;
do
    x = 10;
end
]]
end
str = table.concat(str)

Test { [[
interface I with
    var int x;
end
]]..str..[[

var Class128 instance;
var I* target = &instance;
escape target:x;
]],
    todo = 'crashes',
    complete = true,
    run = 1,
}

-- BUG: cannot contain await nao se aplica a par/or com caminho sem await
Test { [[
input void A,B;

interface I with
    var int v;
    event void inc;
end

class Tx with
    interface I;
do
    await inc;
    this.v = v + 1;
    await FOREVER;
end
pool[] Tx ts;

var int ret = 0;
do
    par/or do
        await B;
    with
        var int i=1;
        every 1s do
            spawn Tx in ts with
                this.v = i;
                i = i + 1;
            end;
        end
    with
        every 1s do
            loop i in ts do
                watching *i do
                    emit i:inc;
                    ret = ret + i:v;
                end
            end
        end
    end
end
escape ret;
]],
    run = { ['~>3s;~>B'] = 13 },
}

-- TODO: como capturar o retorno de um org que termina de imediato?
-- R: option type
Test { [[
class Tx with
do
    escape 1;
end
var Tx*? t = spawn Tx;
var int ret = -1;
if t? then
    ret = await *t!;
end
escape ret;
]],
    run = 1,
}

-- TODO: TRAVERSE C POINTERS
Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;
loop/1 v in [0|>_VS[ do      // think its numeric
    if v == null then
        break;
    else
        ret = ret + v:v;
        traverse v:nxt;
    end
end

escape 1;
]],
    env = 'line 13 : invalid operands to binary "=="',
    --run = 1,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

loop/1 v in &_VS do
    if v == null then
        break;
    else
        ret = ret + v:v;
        traverse v:nxt;
    end
end

escape ret;
]],
    todo = '&_VS cannot be numeric, but it think it is',
    run = 1,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

var _tp* vs = &_VS;
loop/3 v in vs do
    if v == null then
        break;
    else
        ret = ret + v:v;
        traverse v:nxt;
    end
end

escape ret;
]],
    run = 6,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

var _tp* vs = &_VS;
loop/3 v in vs do
    if v == null then
        continue;
    end
    ret = ret + v:v;
    traverse v:nxt;
end

escape ret;
]],
    run = 6,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

var _tp* vs = &_VS;
loop/3 v in vs do
    if v == null then
    else
        ret = ret + v:v;
        traverse v:nxt;
    end
end

escape ret;
]],
    run = 6,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

var _tp* vs = &_VS;
loop/3 v in vs do
    if v == null then
        break;
    else
        traverse v:nxt;
        ret = ret + v:v;
    end
end

escape ret;
]],
    run = 0,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

var _tp* vs = &_VS;
loop/3 v in vs do
    if v == null then
        continue;
    end
    traverse v:nxt;
    ret = ret + v:v;
end

escape ret;
]],
    run = 6,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

var _tp* vs = &_VS;
loop/3 v in vs do
    if v == null then
    else
        traverse v:nxt;
        ret = ret + v:v;
    end
end

escape ret;
]],
    run = 6,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

var _tp* vs = &_VS;
loop/1 v in vs do
    if v == null then
        break;
    else
        ret = ret + v:v;
        traverse v:nxt;
    end
end

escape ret;
]],
    asr = 'runtime error: loop overflow',
}

Test { [[
traverse 1;
]],
    adt = 'line 1 : missing enclosing `traverse´ block',
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;
var int ii  = 0;

var _tp* vs = &_VS;
loop/3 v in vs do
    if v != null then
        var int i = ii;
        ii = ii + 1;
        traverse v:nxt;
        ret = ret + v:v + i;
    end
end

escape ret;
]],
    run = 9,
}
Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;
var int ii  = 0;

var _tp* vs = &_VS;
loop/3 v in vs do
    var int i = ii;
    ii = ii + 1;
    if v != null then
        traverse v:nxt;
        ret = ret + v:v + i;
    end
end

escape ret;
]],
    run = 9,
}

Test { [[
native do
    typedef struct tp {
        int v;
        struct tp* nxt;
    } tp;
    tp V1 = { 1, NULL };
    tp V2 = { 2, &V1  };
    tp VS = { 3, &V2  };
end

var int ret = 0;

var _tp* vs = &_VS;
loop v in vs do
    if v == null then
        break;
    else
        ret = ret + v:v;
        traverse v:nxt;
    end
end

escape ret;
]],
    wrn = true,
    run = 1,
}
