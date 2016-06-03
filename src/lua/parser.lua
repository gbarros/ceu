local P, C, V, S, Cc, Ct = m.P, m.C, m.V, m.S, m.Cc, m.Ct

--[[
local _V = V
local spc = 0
V = function (id)
    return
        m.Cmt(P'',
            function ()
                DBG(string.rep(' ',spc)..'>>>', id)
                spc = spc + 2
                return true
            end)
        * (
            _V(id) * m.Cmt(P'',
                        function ()
                            spc = spc - 2
                            DBG(string.rep(' ',spc)..'+++', id)
                            return true
                        end)
          + m.Cmt(P'',
                function ()
                    spc = spc - 2
                    DBG(string.rep(' ',spc)..'---', id)
                    return false
                end) * P(false)
        )
end
]]

local X = V'__SPACES'

local T = {
    {
        '`%*´ or `/´ or `%%´ or `%+´ or `%-´ or `>>´ or `<<´ or `&´ or `^´ or `|´ or `!=´ or `==´ or `<=´ or `>=´ or `<´ or `>´ or `is´ or `as´ or `and´ or `or´',
        'binary operator'
    },

    {
        '`&&´ or `%?´',
        'type modifier'
    },

    {
        '`&´ or `%(´ or primitive type or abstraction identifier or native identifier',
        'type'
    },
    {
        '`%(´ or primitive type or abstraction identifier or native identifier or `/recursive´',
        'type'
    },
    {
        'primitive type or abstraction identifier or native identifier',
        'type'
    },

    {
        '`pre´ or `native´ or `code/instantaneous´ or `code/delayed´ or end of file',
        'end of file'
    },
    {
        '`;´ or `pre´ or `native´ or `code/instantaneous´ or `code/delayed´ or `with´',
        '`with´'
    },
    {
        '`pre´ or `native´ or `code/instantaneous´ or `code/delayed´ or `end´',
        '`end´'
    },

    {
        '`new´ or abstraction identifier or `emit´ or `call/recursive´ or `call´ or `request´ or `do´ or `await´ or `watching´ or `spawn´ or `async/thread´ or `%[´ or `_´ or `not´ or `%-´ or `%+´ or `~´ or `%*´ or `&&´ or `&´ or `%$%$´ or `%$´ or `%(´ or `sizeof´ or internal identifier or native identifier or `null´ or number or `false´ or `true´ or `"´ or string literal or `global´ or `this´ or `outer´ or `{´',
        'expression'
    },
    {
        'abstraction identifier or `not´ or `%-´ or `%+´ or `~´ or `%*´ or `&&´ or `&´ or `%$%$´ or `%$´ or `%(´ or `sizeof´ or `call´ or `call/recursive´ or internal identifier or native identifier or `null´ or number or `false´ or `true´ or `"´ or string literal or `global´ or `this´ or `outer´ or `{´',
        'expression'
    },
    {
        '`not´ or `%-´ or `%+´ or `~´ or `%*´ or `&&´ or `&´ or `%$%$´ or `%$´ or `%(´ or `sizeof´ or `call´ or `call/recursive´ or abstraction identifier or internal identifier or native identifier or `null´ or number or `false´ or `true´ or `"´ or string literal or `global´ or `this´ or `outer´ or `{´',
        'expression'
    },

    {
        '`nothing´ or `var´ or `vector´ or `pool´ or `event´ or `input´ or `output´ or `data´ or `code/instantaneous´ or `code/delayed´ or `input/output´ or `output/input´ or `native´ or `deterministic´ or expression or `await´ or `emit´ or `request´ or `spawn´ or `kill´ or `pre´ or `do´ or `if´ or `loop´ or `every´ or `par/or´ or `par/and´ or `watching´ or `pause/if´ or `async´ or `async/thread´ or `async/isr´ or `atomic´ or `%[´ or `escape´ or `break´ or `continue´ or `par´ or end of file',
        'statement'
    },
}
if RUNTESTS then
    RUNTESTS.parser_translate = RUNTESTS.parser_translate or { ok={}, original=T }
end

-- ( ) . % + - * ? [ ] ^ $

local function translate (msg)
    for i,t in ipairs(T) do
        local fr,to = unpack(t)
        local new = string.gsub(msg, fr, to)
        if RUNTESTS then
            if msg ~= new then
                RUNTESTS.parser_translate.ok[i] = true
            end
        end
        msg = new
    end
    return msg
end

local ERR_i    = 0
local ERR_strs = {}
local LST_i    = 0
local LST_str  = 'begin of file'

local IGN = 0
local ign_inc   = m.Cmt(P'', function() IGN=IGN+1 return true  end)
local ign_dec_t = m.Cmt(P'', function() IGN=IGN-1 return true  end)
local ign_dec_f = m.Cmt(P'', function() IGN=IGN-1 return false end)

local function I (patt)
    return ign_inc * (patt*ign_dec_t + ign_dec_f*P(false))
end

local function ERR ()
--DBG(LST_i, ERR_i, ERR_strs, _I2L[LST_i], I2TK[LST_i])
    local file, line = unpack(LINES.i2l[LST_i])
    return 'ERR : '..file..
              ' : line '..line..
              ' : after `'..LST_str..'´'..
              ' : expected '..translate(table.concat(ERR_strs,' or '))
end

local function tk_fail (i, err)
    if err == true then
        return false
    end
    if i==ERR_i and (not ERR_strs[err]) then
        ERR_strs[#ERR_strs+1] = err
        ERR_strs[err] = true
    elseif i > ERR_i then
        ERR_i = i
        ERR_strs = { err }
        ERR_strs[err] = true
    end
    return false
end

-- KK accepts leading chars
local function KK (patt, err, nox)
    if type(patt) == 'string' then
        err = err or '`'..patt..'´'
    else
        err = err or error(debug.traceback())
    end

    local ret = m.Cmt(patt,
                    -- SUCCESS
                    function (_, i, tk)
                        if IGN>0 then return true end
                        if i > LST_i then
                            LST_i   = i
                            LST_str = tk
                        end
                        return true
                    end)
              + m.Cmt(P'',
                    -- FAILURE
                    function (_,i)
                        if IGN>0 then return false end
                        return tk_fail(i,err)
                    end) * P(false)
                           -- (avoids "left recursive" error (explicit fail))

    if not nox then
        ret = ret * X
    end
    return ret
end

-- K is exact match
local function K (patt, err, nox)
    err = err or '`'..patt..'´'
    patt = patt * -m.R('09','__','az','AZ','\127\255')
    return KK(patt, err, nox)
end

local CKK = function (tk,err)
    return C(KK(tk,err,true)) * X
end
local CK = function (tk,err)
    return C(K(tk,err,true)) * X
end

local OPT = function (patt)
    return patt + Cc(false)
end

local PARENS = function (patt)
    return KK'(' * patt * KK')'
end

local Ccs = function (...)
    local ret = Cc(true)
    for _, v in ipairs(...) do
        ret = ret * Cc(v)
    end
    return ret
end

local E = function (msg)
    return m.Cmt(P'',
            function (_,i)
                return tk_fail(i,msg)
            end)
end

--[[
local function v_fail (i, err, idx, dbg)
    if i==ERR_i and (not ERR_strs[err]) then
        for i=#ERR_strs, idx, -1 do
            local err2 = ERR_strs[i]
            ERR_strs[i]    = nil
            ERR_strs[err2] = nil
        end
        ERR_strs[#ERR_strs+1] = err
        ERR_strs[err] = true
    end
    return false
end

local _V = V
local function V (rule, err)
    local patt = _V(rule)
-- DBG
--err = true
    if not err then
        return patt
    end
    local idx
    return m.Cmt(P'', function ()
                        idx = #ERR_strs+1
                        return true
                      end)
            * patt
               + m.Cmt(P'',
                    -- FAILURE
                    function (_,i)
                        if IGN>0 then return false end
-- DBG
                        return v_fail(i, err, idx, err==true and rule)
                    end) * P(false)
                           -- (avoids "left recursive" error (explicit fail))
end
]]

-->>> OK
TYPES = P'bool' + 'byte'
      + 'f32' + 'f64' + 'float'
      + 'int'
      + 's16' + 's32' + 's64' + 's8'
      + 'ssize'
      + 'u16' + 'u32' + 'u64' + 'u8'
      + 'uint' + 'usize' + 'void'
--<<<

-- must be in reverse order (to count superstrings as keywords)
KEYS = P
'with' +
'watching' +
'vector' +
'var' +
'until' +
'true' +
'this' +
'then' +
'spawn' +
'sizeof' +
'request' +
'pre' +
'pool' +
'pause/if' +
'par/or' +
'par/and' +
'par' +
'output/input' +
'output' +
'outer' +
'or' +
'null' +
'nothing' +
'not' +
'new' +
'native' +
'loop' +
'kill' +
'is' +
-- TODO: remove class/interface
'class' + 'interface' + 'traverse' +
'input/output' +
'input' +
'in' +
'if' +
'global' +
'FOREVER' +
'finalize' +
'false' +
'every' +
'event' +
'escape' +
'end' +
'emit' +
'else/if' +
'else' +
'do' +
'deterministic' +
'data' +
'continue' +
'code' +
'call/recursive' +
'call' +
'break' +
'await' +
'atomic' +
'async/thread' +
'async/isr' +
'async' +
'as' +
'and' +
TYPES

KEYS = KEYS * -m.R('09','__','az','AZ','\127\255')

local Alpha    = m.R'az' + '_' + m.R'AZ'
local Alphanum = Alpha + m.R'09'
local ALPHANUM = m.R'AZ' + '_' + m.R'09'
local alphanum = m.R'az' + '_' + m.R'09'

-- Rule:    unchanged in the AST
-- _Rule:   changed in the AST as "Rule"
-- __Rule:  container for other rules, not in the AST
-- __rule:  (local) container for other rules

GG = { [1] = X * V'_Stmts' * (P(-1) + E('end of file'))

-->>> OK

    , __seqs = KK';' * KK(';',true)^0     -- "true": ignore as "expected"
    , Nothing = K'nothing'

-- DO, BLOCK

    -- escape/A 10
    -- break/i
    -- continue/i
    , _Escape   = K'escape'   * OPT('/'*V'__ID_esc')
                              * OPT(V'__Exp')
    , _Break    = K'break'    * OPT('/'*V'ID_int')
    , _Continue = K'continue' * OPT('/'*V'ID_int')

    -- do/A ... end
    , Do = K'do' * OPT('/'*V'__ID_esc') *
                V'Block' *
           K'end'

    , __Do  = K'do' * V'Block' * K'end'
    --, _Dopre = K'pre' * V'__Do'

    , Block = V'_Stmts'

-- PAR, PAR/AND, PAR/OR

    , Par    = K'par' * K'do' *
                V'Block' * (K'with' * V'Block')^1 *
               K'end'
    , Parand = K'par/and' * K'do' *
                V'Block' * (K'with' * V'Block')^1 *
               K'end'
    , Paror  = K'par/or' * K'do' *
                V'Block' * (K'with' * V'Block')^1 *
               K'end'

-- FLOW CONTROL

    , If = K'if' * V'__Exp_Bool' * K'then' * V'Block' *
           (K'else/if' * V'__Exp_Bool' * K'then' * V'Block')^0 *
           OPT(K'else' * V'Block') *
           K'end'

    , Loop       = K'loop' * OPT('/'*V'__Exp_Num') *
                   V'__Do'
    , _Loop_Num  = K'loop' * OPT('/'*V'__Exp_Num') *
                    (V'__ID_int'+V'ID_none') * OPT(
                        K'in' * (CKK'[' + CKK']') * (
                                V'__Exp_Num' * CKK'->' * (V'ID_none' + V'__Exp_Num') +
                                (V'ID_none' + V'__Exp_Num') * CKK'<-' * V'__Exp_Num'
                            ) * (CKK'[' + CKK']') *
                            OPT(KK',' * V'__Exp_Num')
                    ) *
                   V'__Do'
    , _Loop_Pool = K'loop' * OPT('/'*V'__Exp_Num') *
                    (V'ID_int'+V'ID_none') * K'in' * V'__Exp_Field' *
                   V'__Do'

    , _Every  = K'every' * OPT((V'ID_int'+PARENS(V'Varlist')) * K'in') *
                    (V'__awaits'-I(V'Await_Code')) *
                V'__Do'

    , CallStmt = V'__Exp_Call'

--[[
    , Finalize = K'do' *
                    V'Block' *
                 K'finalize' * OPT(V'Explist') * K'with' *
                    V'Block' *
                 K'end'

    , _Pause   = K'pause/if' * V'__Exp_Field' * V'__Do'

-- ASYNCHRONOUS

    , Async   = K'async' * (-P'/thread'-'/isr') * OPT(PARENS(V'Varlist')) *
                V'__Do'
    , _Thread = K'async/thread' * OPT(PARENS(V'Varlist')) * V'__Do'
    , _Isr    = K'async/isr'    *
                    KK'[' * V'Explist' * KK']' *
                    OPT(PARENS(V'Varlist')) *
                V'__Do'
]]
    , Atomic  = K'atomic' * V'__Do'

-- CODE / EXTS (call, req)

    -- CODE

    , __code   = (CK'code/instantaneous' + CK'code/delayed')
                    * OPT(CK'/recursive')
                    * V'__ID_abs'
    , Code_proto = V'__code' * (V'Typepars_ids'+V'Typepars_anon') *
                                    KK'=>' * V'Type'
    , Code_impl  = V'__code' * V'Typepars_ids' *
                                    KK'=>' * V'Type' *
                   V'__Do'

    , _Spawn_Block = K'spawn' * V'__Do'
    , Spawn_Code   = K'spawn' * V'CALL'

    -- EXTS

    -- call
    , __extcall = (CK'input' + CK'output')
                    * OPT(CK'/recursive')
                    * V'__ID_ext'
    , Extcall_proto = V'__extcall' * (V'Typepars_ids'+V'Typepars_anon') *
                                        KK'=>' * V'Type'
    , Extcall_impl  = V'__extcall' * V'Typepars_ids' *
                                        KK'=>' * V'Type' *
                       V'__Do'

    -- req
    , __extreq = (CK'input/output' + CK'output/input')
                   * OPT('[' * (V'__Exp_Num'+Cc(true)) * KK']')
                   * V'__ID_ext'
    , _Extreq_proto = V'__extreq' * (V'Typepars_ids'+V'Typepars_anon') *
                                        KK'=>' * V'Type'
    , _Extreq_impl  = V'__extreq' * V'Typepars_ids' *
                                        KK'=>' * V'Type' *
                      V'__Do'

    -- TYPEPARS

    -- (var& int, var/nohold void&&)
    -- (var& int v, var/nohold void&& ptr)
    , __typepars_pre = CK'vector' * CKK'&' * V'__Dim'
                     + CK'pool'   * CKK'&' * V'__Dim'
                     + CK'event'  * CKK'&'
                     + CK'var'   * OPT(CKK'&') * OPT(KK'/'*CK'hold')

    , Typepars_ids_item  = V'__typepars_pre' * V'Type' * V'__ID_int'
    , Typepars_anon_item = V'__typepars_pre' * V'Type'

    , Typepars_ids = #KK'(' * (
                    PARENS(P'void') +
                    PARENS(V'Typepars_ids_item'   * (KK','*V'Typepars_ids_item')^0)
                  )
    , Typepars_anon = #KK'(' * (
                    PARENS(P'void') +
                    PARENS(V'Typepars_anon_item' * (KK','*V'Typepars_anon_item')^0)
                  )

-- DATA

    , __data       = K'data' * V'__ID_abs' * OPT(K'is' * V'ID_abs')
    , _Data_simple = V'__data'
    , _Data_block  = V'__data' * K'with' * (
                        (V'_Vars'+V'_Vecs'+V'_Pools'+V'_Evts') *
                            V'__seqs'
                     )^1 * K'end'

-- NATIVE, C, LUA

    -- C

    , _Nats  = K'native' *
                    OPT(KK'/'*(CK'pure'+CK'const'+CK'nohold'+CK'plain')) *
                        V'__ID_nat' * (KK',' * V'__ID_nat')^0
        --> Nat+

    , Nat_End = K'native' * KK'/' * K'end'
    , Nat_Block = OPT(CK'pre') * K'native' * (#K'do')*'do' *
                ( C(V'_C') + C((P(1)-(S'\t\n\r '*'end'*P';'^0*'\n'))^0) ) *
             X* K'end'

    , Nat_Stmt = KK'{' * C(V'__nat') * KK'}'
    , Nat_Exp  = KK'{' * C(V'__nat') * KK'}'
    , __nat   = ((1-S'{}') + '{'*V'__nat'*'}')^0

    -- Lua

    , _Lua     = KK'[' * m.Cg(P'='^0,'lua') * KK'[' *
                 ( V'__luaext' + C((P(1)-V'__luaext'-V'__luacmp')^1) )^0
                  * (V'__luacl'/function()end) *X
    , __luaext = P'@' * V'__Exp'
    , __luacl  = ']' * C(P'='^0) * KK']'
    , __luacmp = m.Cmt(V'__luacl' * m.Cb'lua',
                    function (s,i,a,b) return a == b end)

-- VARS, VECTORS, POOLS, VTS, EXTS

    -- DECLARATIONS

    , __vars_set  = V'__ID_int' * OPT(Ct(V'__Sets_one'))

    , _Vars_set  = K'var' * OPT(CKK'&') * V'Type' *
                    V'__vars_set' * (KK','*V'__vars_set')^0
    , _Vars      = K'var' * OPT(CKK'&') * V'Type' *
                    V'__ID_int' * (KK','*V'__ID_int')^0

    , _Vecs_set  = K'vector' * OPT(CKK'&') * V'__Dim' * V'Type' *
                    V'__vars_set' * (KK','*V'__vars_set')^0
                        -- TODO: only vec constr
    , _Vecs      = K'vector' * OPT(CKK'&') * V'__Dim' * V'Type' *
                    V'__ID_int' * (KK','*V'__ID_int')^0

    , _Pools_set = K'pool' * OPT(CKK'&') * V'__Dim' * V'Type' *
                    V'__vars_set' * (KK','*V'__vars_set')^0
    , _Pools     = K'pool' * OPT(CKK'&') * V'__Dim' * V'Type' *
                    V'__ID_int' * (KK','*V'__ID_int')^0

    , _Evts_set  = K'event' * OPT(CKK'&') * (PARENS(V'_Typelist')+V'Type') *
                    V'__vars_set' * (KK','*V'__vars_set')^0
    , _Evts      = K'event' * OPT(CKK'&') * (PARENS(V'_Typelist')+V'Type') *
                    V'__ID_int' * (KK','*V'__ID_int')^0

    , _Exts      = (CK'input'+CK'output') * (PARENS(V'_Typelist')+V'Type') *
                    V'__ID_ext' * (KK','*V'__ID_ext')^0

-- AWAIT, EMIT

    , __awaits     = (V'Await_Ext' + V'Await_Evt' + V'Await_Wclock' + V'Await_Code')
    , _Awaits      = K'await' * V'__awaits' * OPT(K'until'*V'__Exp_Bool')
    , Await_Ext    = V'ID_ext' - V'Await_Code'
    , Await_Evt    = V'__Exp_Lval' - V'Await_Wclock' - V'Await_Code'
    , Await_Wclock = (V'WCLOCKK' + V'WCLOCKE')
    , Await_Code   = V'CALL'

    , Await_Forever = K'await' * K'FOREVER'

    , __evts_ps = V'__Exp' + PARENS(OPT(V'Explist'))
    , Emit_Ext_emit = K'emit' * (
                        (V'WCLOCKK'+V'WCLOCKE') +
                        V'ID_ext' * OPT(KK'=>' * V'__evts_ps')
                      )
    , Emit_Ext_call = (K'call/recursive'+K'call') *
                        V'ID_ext' * OPT(KK'=>' * V'__evts_ps')
    , Emit_Ext_req  = K'request' *
                        V'ID_ext' * OPT(KK'=>' * V'__evts_ps')

    , Emit_Evt = K'emit' * -#(V'WCLOCKK'+V'WCLOCKE') *
                    V'__Exp_Field' * OPT(KK'=>' * V'__evts_ps')

    , _Watching = K'watching' * V'__awaits' * V'__Do'

    , __num = CKK(m.R'09'^1,'number') / tonumber
    , WCLOCKK = #V'__num' *
                (V'__num' * P'h'   *X + Cc(0)) *
                (V'__num' * P'min' *X + Cc(0)) *
                (V'__num' * P's'   *X + Cc(0)) *
                (V'__num' * P'ms'  *X + Cc(0)) *
                (V'__num' * P'us'  *X + Cc(0)) *
                (V'__num' * E'<h,min,s,ms,us>')^-1
                    * OPT(CK'/_')
    , WCLOCKE = PARENS(V'__Exp_Num') * (
                    CK'h' + CK'min' + CK's' + CK'ms' + CK'us'
                  + E'<h,min,s,ms,us>'
              ) * OPT(CK'/_')

-- DETERMINISTIC

    , __det_id = V'ID_ext' + V'ID_int' + V'ID_abs' + V'ID_nat'
    , Deterministic = K'deterministic' * V'__det_id' * (
                        K'with' * V'__det_id' * (KK',' * V'__det_id')^0
                      )^-1

-- SETS

    , _Set_one   = V'__Exp_Lval'      * V('__Sets_one')
    , _Set_many  = PARENS(V'Varlist') * V'__Sets_many'

    , __Sets_one  = ((CKK'='+CKK':=')-'==') * (V'__sets_one'  + PARENS(V'__sets_one'))
    , __Sets_many = ((CKK'='+CKK':=')-'==') * (V'__sets_many' + PARENS(V'__sets_many'))

    , __sets_one =
--[[
          V'_Set_Data'
        + V'_Set_Emit_Ext_emit' + V'_Set_Emit_Ext_call' + V'_Set_Emit_Ext_req'
        + V'_Set_Do'
        + V'_Set_Await'
        + V'_Set_Watching'
        + V'_Set_Spawn'
        + V'_Set_Thread'
        + V'_Set_Lua'
        + V'_Set_Vec'
        + V'_Set_None'
]]
          P(false)
        + V'_Set_Exp'

    --, __sets_many = V'_Set_Emit_Ext_req' + V'_Set_Await' + V'_Set_Watching'
    , __sets_many = P(false)

    -- after `=´

--[[
    , _Set_Do       = #(K'do'*KK'/')    * V'Do'
    , _Set_Await    = #K'await'         * V'_Awaits'
    , _Set_Watching = #K'watching'      * V'_Watching'
    , _Set_Spawn    = #K'spawn'         * V'Spawn_Code'
    , _Set_Thread   = #K'async/thread'  * V'_Thread'
    , _Set_Lua      = #V'__lua_pre'     * V'_Lua'
    , _Set_Vec      = #V'__vec_pre'     * V'_Vec_New'
    , _Set_Data     = #V'__data_pre'    * V'Data_New'
    , _Set_None     = #K'_'             * V'ID_none'
]]
    , _Set_Exp      =                     V'__Exp'

    , _Set_Emit_Ext_emit  = #K'emit'          * V'Emit_Ext_emit'
    , _Set_Emit_Ext_req   = #K'request'       * V'Emit_Ext_req'
    , _Set_Emit_Ext_call  = #V'__extcall_pre' * V'Emit_Ext_call'

    , __extcall_pre = (K'call/recursive'+K'call') * V'ID_ext'
    , __lua_pre     = KK'[' * (P'='^0) * '['
    , __vec_pre     = KK'[' - V'__lua_pre'
    , __data_pre    = K'new'^-1 * V'__ID_abs'

    , Vec_Tup  = V'__vec_pre' * OPT(V'Explist') * KK']'
    , _Vec_New = V'Vec_Tup' * (KK'..' * (
                    (V'__Exp_Field'+V'__Exp_Ptr'+'__Exp_Nat'+V'STRING') + #KK'['*V'Vec_Tup')
                    --(V'__Exp') + #KK'['*V'Vec_Tup')
                 )^0

    , Data_New = OPT(CK'new') * V'Data_New_one'
    , Data_New_one  = V'ID_abs' * PARENS(V'_Data_explist')
    , _Data_explist    = ( V'__data_expitem'*(KK','*V'__data_expitem')^0 )^-1
    , __data_expitem   = (V'Data_New_one' + V'_Vec_New' + V'__Exp')

-- IDS

    , ID_prim = V'__ID_prim'
    , ID_ext  = V'__ID_ext'
    , ID_int  = V'__ID_int'
    , ID_abs  = V'__ID_abs'
    , ID_nat  = V'__ID_nat'
    , ID_none = V'__ID_none'

    , __ID_prim = CK(TYPES,                     'primitive type')
    , __ID_ext  = CK(m.R'AZ'*ALPHANUM^0  -KEYS, 'external identifier')
    , __ID_int  = CK(m.R'az'*Alphanum^0  -KEYS, 'internal identifier')
    , __ID_abs  = CK(m.R'AZ'*V'__one_az' -KEYS, 'abstraction identifier')
    , __ID_nat  = CK(P'_' * Alphanum^1,         'native identifier')
    , __ID_none = CK(P'_' * -Alphanum,          '`_´')
    , __ID_esc  = CK(Alpha*(Alphanum)^0 -KEYS,  '`escape´ identifier')

    -- at least one lowercase character
    , __one_az = #(ALPHANUM^0*m.R'az') * Alphanum^0


-- MODS

    , __Dim = KK'[' * (V'__Exp_Num'+Cc('[]')) * KK']'

-- LISTS

    , Varlist   = V'ID_int' * (KK',' * V'ID_int')^0
    , Explist   = V'__Exp'  * (KK',' * V'__Exp')^0
    , _Typelist = V'Type'   * (KK',' * V'Type')^0

 --<<<

    , Kill  = K'kill' * V'__Exp_Lval' * OPT(KK'=>'*V'__Exp')

-- Types

    , __type = V'ID_prim' + V'ID_abs'
    , __type_ptr = CKK'&&' -(P'&'^3)
    , __type_vec = KK'[' * V'__Exp_Num' * KK']'
    , Type = V'__type' * (V'__type_ptr'              )^0 * CKK'?'^-1
           + V'ID_nat' * (V'__type_ptr'+V'__type_vec')^0 * CKK'?'^-1

    , CALL   = V'ID_abs' * PARENS(OPT(V'Explist'))
    , SIZEOF = K'sizeof' * PARENS((V'Type' + V'__Exp'))
    , NULL   = CK'null'     -- TODO: the idea is to get rid of this
    , STRING = CKK( CKK'"' * (P(1)-'"'-'\n')^0 * K'"', 'string literal' )

    , NUMBER = CK( #m.R'09' * (m.R'09'+S'xX'+m.R'AF'+m.R'af'+(P'.'-'..')
                                      +(S'Ee'*'-')+S'Ee')^1,
                   'number' )
             + CKK( "'" * (P(1)-"'")^0 * "'" , 'number' )

    , BOOLEAN   = K'false' / function() return 0 end
             + K'true'  / function() return 1 end

    , Global  = K'global'
    , This    = K'this' * Cc(false)
    , Outer   = K'outer'

---------
                -- "Ct" as a special case to avoid "too many captures" (HACK_1)
    , _Stmts  = Ct (( V'__Stmt_Simple' * V'__seqs' +
                      --V'__Stmt_Block' * (KK';'^0)
                      P(false)
                   )^0
                 * ( V'__Stmt_Last' * V'__seqs' +
                     V'__Stmt_Last_Block' * (KK';'^0)
                   )^-1
                 * (V'Nat_Block'+V'Code_impl')^0 )

    , __Stmt_Last  = V'_Escape' + V'_Break' + V'_Continue' + V'Await_Forever'
    , __Stmt_Last_Block = V'Par'
    , __Stmt_Simple    = V'Nothing'
                 + V'_Vars_set'  + V'_Vars'
                 + V'_Vecs_set'  + V'_Vecs'
                 + V'_Pools_set' + V'_Pools'
                 + V'_Evts_set'  + V'_Evts'
                 + V'_Exts'
                 + V'_Data_simple'
                 + V'Code_proto' + V'Extcall_proto' + V'_Extreq_proto'
                 + V'_Nats'  + V'Nat_End'
                 + V'Deterministic'
                 + V'_Set_one' + V'_Set_many'
                 + V'_Awaits'
                 + V'Emit_Ext_emit' + V'Emit_Ext_call' + V'Emit_Ext_req'
                 + V'Emit_Evt'
                 + V'Spawn_Code' + V'Kill'
                 + V'Nat_Stmt'
-- TODO: remove class/interface
+ I((K'class'+K'interface'+K'traverse')) * E'TODO: class/interface'
             + V'CallStmt' -- last

--[[
    , __Stmt_Block = V'Code_impl' + V'Extcall_impl' + V'_Extreq_impl'
              + V'_Data_block'
              + V'Nat_Block'
              + V'Do'    + V'If'
              + V'Loop' + V'_Loop_Num' + V'_Loop_Pool'
              + V'_Every'
              + V'_Spawn_Block'
              + V'Finalize'
              + V'Paror' + V'Parand' + V'_Watching'
              + V'_Pause'
              + V'Async' + V'_Thread' + V'_Isr' + V'Atomic'
              + V'_Dopre'
              + V'_Lua'
]]

    --, _C = '/******/' * (P(1)-'/******/')^0 * '/******/'
    , _C      = m.Cg(V'_CSEP','mark') *
                    (P(1)-V'_CEND')^0 *
                V'_CEND'
    , _CSEP = '/***' * (1-P'***/')^0 * '***/'
    , _CEND = m.Cmt(C(V'_CSEP') * m.Cb'mark',
                    function (s,i,a,b) return a == b end)

    , __SPACES = (('\n' * (V'__comm'+S'\t\n\r ')^0 *
                    '#' * (P(1)-'\n')^0)
                + ('//' * (P(1)-'\n')^0)
                + S'\t\n\r '
                + V'__comm'
                )^0

    , __comm    = '/' * m.Cg(P'*'^1,'comm') * (P(1)-V'__commcmp')^0 * 
                    V'__commcl'
                    / function () end
    , __commcl  = C(P'*'^1) * '/'
    , __commcmp = m.Cmt(V'__commcl' * m.Cb'comm',
                    function (s,i,a,b) return a == b end)

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

-- CALL

--[=[
]=]
    , __Exp_Call  = V'__1_Call'
--[[
]]
    , __1_Call    = V'__2_Call'
    , __2_Call    = V'__3_Call'
    , __3_Call    = V'__4_Call'
    , __4_Call    = V'__5_Call'
    , __5_Call    = V'__6_Call'
    , __6_Call    = V'__7_Call'
    , __7_Call    = V'__8_Call'
    , __8_Call    = V'__9_Call'
    , __9_Call    = V'__10_Call'
    , __10_Call   = V'__11_Call'
    , __11_Call   = V'__12_Lval' * V'__call'
                  + V'__12_Call'
    , __12_Call   = PARENS(V'__Exp_Call')
                  + (CK'call' + CK'call/recursive' + Cc'call') * V'CALL'
                  + CK'call'           * V'__Exp_Call'
                  + CK'call/recursive' * V'__Exp_Call'

-- FIELD

    , __Exp_Field  = V'__1_Field'
--[[
]]
    , __1_Field    = V'__2_Field'
    , __2_Field    = V'__3_Field'
    , __3_Field    = V'__4_Field' * V'__cast'^0
    , __4_Field    = V'__5_Field'
    , __5_Field    = V'__6_Field'
    , __6_Field    = V'__7_Field'
    , __7_Field    = V'__8_Field'
    , __8_Field    = V'__9_Field'
    , __9_Field    = V'__10_Field'
    , __10_Field   = V'__11_Field'
    , __11_Field   = V'__12_Lval' * (V'__field')^1
                   + V'__12_Field'
    , __12_Field   = PARENS(V'__12_Field') + V'ID_int'

-- LVAL

    , __Exp_Lval  = V'__1_Lval'
    , __1_Lval    = V'__2_Lval'
    , __2_Lval    = V'__3_Lval'
    , __3_Lval    = V'__4_Lval' * V'__cast'^0
    , __4_Lval    = V'__5_Lval'
    , __5_Lval    = V'__6_Lval'
    , __6_Lval    = V'__7_Lval'
    , __7_Lval    = V'__8_Lval'
    , __8_Lval    = V'__9_Lval'
    , __9_Lval    = V'__10_Lval'
    , __10_Lval   = (Cc(false) * (CKK'$'-'$$'))^-1 * (Cc(false) * CKK'*')^1 *
                                                        (V'__11_Ptr' + V'__11_Nat')
                  + (Cc(false) * (CKK'$'-'$$'))^-1 * V'__11_Lval'
    , __11_Lval   = V'__12_Lval' * (V'__idx' + V'__field' + V'__opt_get')^0
    , __12_Lval   = PARENS(V'__Exp_Lval')
                  + V'ID_int'  + V'ID_nat'
                  + V'Global'  + V'This'   + V'Outer'
                  + V'Nat_Exp'
--[[
]]

-- PTR

    , __Exp_Ptr  = V'__1_Ptr'
--[[
]]
    , __1_Ptr    = V'__2_Ptr'
    , __2_Ptr    = V'__3_Ptr'
    , __3_Ptr    = (V'__4_Ptr'+V'__4_Nat') * V'__cast'^0
    , __4_Ptr    = V'__5_Ptr'
    , __5_Ptr    = V'__6_Ptr'
    , __6_Ptr    = V'__7_Ptr'
    , __7_Ptr    = V'__8_Ptr'
    --, __8_Ptr    = V'__9_Ptr'
    , __8_Ptr    = V'__9_Ptr' * ((CKK'+'+CKK'-') * V'__9_Num')^0
    , __9_Ptr    = V'__10_Ptr'
    , __10_Ptr   = ( Cc(false) * (CKK'*' + (CKK'&&'-P'&'^3)))^1 * V'__11_Lval'
                 + V'__11_Ptr'
    , __11_Ptr   = V'__12_Lval' * (V'__call'  + V'__idx' +
                                   V'__field' + V'__opt_get')^1
                 + V'__12_Lval'
                 + V'__12_Call'
                 + V'__12_Ptr'
    , __12_Ptr   = PARENS(V'__Exp_Ptr') + V'NULL' + V'STRING'

-- BOOL

    , __b     = (V'__4_Bool' + V'__b_num' + V'__b_ptr' + V'__b_is' + V'__b_as')
    , __b_num = V'__4_Num' * ( (CKK'!='-'!==')+CKK'=='+CKK'<='+CKK'>='
                               + (CKK'<'-'<<')+(CKK'>'-'>>')
                               ) * V'__4_Num'
    , __b_ptr = V'__4_Ptr' * ((CKK'!='-'!==')+CKK'==') * V'__4_Ptr'
    --, __b_is  = P(false)
    --, __b_as  = P(false)
    , __b_is  = V'__4' * (CK'is' * V'Type')^1
    , __b_as  = V'__4' * (#(K'as'*K'bool') * V'__cast')^1
--[[
]]

    , __Exp_Bool = V'__1_Bool'
    , __1_Bool   = V'__2_Bool' * (CK'or'  * V'__2_Bool')^0
    , __2_Bool   = V'__3_Bool' * (CK'and' * V'__3_Bool')^0
    --, __3_Bool   = P(false)
    --, __3_Bool   = V'__b_num'
    , __3_Bool   = V'__b' * (((CKK'!='-'!==')+CKK'==') * V'__b')^0
    , __4_Bool   = V'__5_Bool'
    , __5_Bool   = V'__6_Bool'
    , __6_Bool   = V'__7_Bool'
    , __7_Bool   = V'__8_Bool'
    , __8_Bool   = V'__9_Bool'
    , __9_Bool   = V'__10_Bool'
    , __10_Bool  = (Cc(false) * CK'not')^0 * (Cc(false) * CKK'*')^1 *
                                                (V'__11_Ptr' + V'__11_Nat')
                 + (Cc(false) * CK'not')^0 * V'__11_Bool'
    , __11_Bool  = V'__12_Lval' * (V'__call' + V'__idx' + V'__field' +
                                   V'__opt_get' + V'__opt_ask')^1
                 + V'__12_Lval'
                 + V'__12_Call'
                 + V'__12_Bool'
    , __12_Bool  = PARENS(V'__Exp_Bool') + V'BOOLEAN'
--[[
]]

-- NUM

    , __Exp_Num  = V'__1_Num'
    , __1_Num    = V'__2_Num'
    , __2_Num    = V'__3_Num'
    , __3_Num    = (V'__4_Num'+V'__4_Nat') * V'__cast'^0
    , __4_Num    = V'__5_Num'   * ((CKK'|'-'||') * V'__5_Num')^0
    , __5_Num    = V'__6_Num'   * (CKK'^' * V'__6_Num')^0
    , __6_Num    = V'__7_Num'   * (CKK'&' * V'__7_Num')^0
    , __7_Num    = V'__8_Num'   * ((CKK'>>'+CKK'<<') * V'__8_Num')^0
    , __8_Num    = V'__9_Num'   * ((CKK'+'+CKK'-') * V'__9_Num')^0
    , __9_Num    = V'__10_Num'  * ((CKK'*'+(CKK'/'-'//'-'/*')+CKK'%') * V'__10_Num')^0
    , __10_Num   = (Cc(false)   * (CKK'-'+CKK'+'+CKK'~'+CKK'*'))^0    * V'__11_Num'
                 + (Cc(false)   * (CKK'$$' + (CKK'$'-'$$')))^0 * V'__11_Lval'
    , __11_Num   = V'__12_Lval' * (V'__call'  + V'__idx' +
                                   V'__field' + V'__opt_get')^1
                 + V'__12_Lval'
                 + V'__12_Call'
                 + V'__12_Num'
    , __12_Num   = PARENS(V'__Exp_Num') + V'SIZEOF' + V'NUMBER'
--[[
]]

-- NAT

    , __Exp_Nat  = V'__1_Nat'
    , __1_Nat    = V'__2_Nat'
    , __2_Nat    = V'__3_Nat'
    , __3_Nat    = V'__4_Nat' * (#(K'as'*'_') * V'__cast')^0
    , __4_Nat    = V'__5_Nat'
    , __5_Nat    = V'__6_Nat'
    , __6_Nat    = V'__7_Nat'
    , __7_Nat    = V'__8_Nat'
    , __8_Nat    = V'__9_Nat'
    , __9_Nat    = V'__10_Nat'
    , __10_Nat   = ( Cc(false) * (CKK'&&'-P'&'^3))^0 * V'__11_Nat'
    , __11_Nat   = V'__12_Nat' * (V'__call'  + V'__idx' +
                                  V'__field' + V'__opt_get')^0
    , __12_Nat   = PARENS(V'__Exp_Nat') + V'ID_nat' + V'Nat_Exp'
--[[
]]

-- EXP

    , __Exp = V'__Exp_Nat' + V'__Exp_Num' + V'__Exp_Bool'
            + V'__Exp_Ptr' + V'__Exp_Lval' + V'__Exp_Call'
    , __1   = P(false)
    , __2   = P(false)
    , __3   = P(false)
    --, __4   = P(false)
    , __4   = V'__4_Nat' + V'__4_Num' + V'__4_Bool'
            + V'__4_Ptr' + V'__4_Lval' + V'__4_Call'
    , __5   = P(false)
    , __6   = P(false)
    , __7   = P(false)
    , __8   = P(false)
    , __9   = P(false)
    , __10  = P(false)
    , __11  = P(false)
    , __12  = P(false)

--[[
    , __Exp_Nat  = V'__Exp'
    , __Exp_Num  = V'__Exp'
    , __Exp_Bool = V'__Exp'
    , __Exp_Ptr  = V'__Exp'
    , __Exp_Lval = V'__Exp'
    , __Exp_Call = V'__Exp'
    , __Exp_Field = V'__Exp'

    , __Exp  = V'__1'
    , __1    = V'__2'  * (CK'or'  * V'__2')^0
    , __2    = V'__3'  * (CK'and' * V'__3')^0
    , __3    = V'__4'  * ( ( (CKK'!='-'!==')+CKK'=='+CKK'<='+CKK'>='
                           + (CKK'<'-'<<')+(CKK'>'-'>>')
                           ) * V'__4'
                         + CK'is' * V'Type'
                         + V'__cast'
                         )^0
    , __4    = V'__5'  * ((CKK'|'-'||') * V'__5')^0
    , __5    = V'__6'  * (CKK'^' * V'__6')^0
    , __6    = V'__7'  * (CKK'&' * V'__7')^0
    , __7    = V'__8'  * ((CKK'>>'+CKK'<<') * V'__8')^0
    , __8    = V'__9'  * ((CKK'+'+CKK'-') * V'__9')^0
    , __9    = V'__10' * ((CKK'*'+(CKK'/'-'//'-'/*')+CKK'%') * V'__10')^0
    , __10   = ( Cc(false) * ( CK'not'+CKK'-'+CKK'+'+CKK'~'+CKK'*'+
                               (CKK'&&'-P'&'^3) + (CKK'&'-'&&') +
                               CKK'$$' + (CKK'$'-'$$') )
               )^0 * V'__11'
    , __11   = V'__12' * (V'__call'  + V'__idx' +
                          V'__field' + V'__opt_get' + V'__opt_ask')^0
    , __12   = PARENS(V'__Exp')
             + V'SIZEOF'
             + (CK'call' + CK'call/recursive' + Cc'call') * V'CALL'
             + V'ID_int'  + V'ID_nat'
             + V'NULL'    + V'NUMBER' + V'BOOLEAN' + V'STRING'
             + V'Global'  + V'This'   + V'Outer'
             + V'Nat_Exp'
             + CK'call'           * V'__Exp'
             + CK'call/recursive' * V'__Exp'
]]

--

    , __call    = PARENS(Cc'call' * OPT(V'Explist'))
    --, __idx     = KK'[' * Cc'idx' * V'__Exp' * KK']'
    , __idx     = KK'[' * Cc'idx' * V'__Exp_Num' * KK']'
    , __field   = (CKK':'+(CKK'.'-'..')) * (V'__ID_int'+V'__ID_nat')
    , __opt_get = (CKK'!'-'!=')
    , __opt_ask = CKK'?'
    , __cast    = CK'as' * (V'Type' + KK'/'*(CK'nohold'+CK'plain'+CK'pure'))
}

if RUNTESTS then
    assert(m.P(GG):match(OPTS.source), ERR())
else
    if not m.P(GG):match(OPTS.source) then
             -- TODO: match only in ast.lua?
        DBG(ERR())
        os.exit(1)
    end
end
