F = {
-- IMPORT: ignore top-level stmts & Global
--[[
    Node = function (me)
        -- ignore top-level stmts, except "Host"
        local inc = _AST.iter'Import'()  -- inc[1] = Stmts #HOLE
        if inc then
            if me.tag == 'Host' then
                inc.__par[#inc.__par+1] = me
            end
            return _AST.node('Nothing')(me.ln,false)
        end
    end,
    Dcl_cls = function (me)
        -- ignore Global if url~=main
        if me[3]=='Global' and me.ln[1]~=_AST.root.ln[1] then
            return _AST.node('Nothing')(me.ln)
        end
    end,
]]
---

-- AWAIT: await x until y (not child from SetAwait)
    AwaitT = function (me)
        if me.setto then
            return      -- already handled by SetAwait
        end
        local ret = _AST.SetAwaitUntil(me.ln, me)
        return ret
    end,
    AwaitExt = 'AwaitT',
    AwaitInt = 'AwaitT',
    AwaitS   = 'AwaitT',
---

-- FINALIZE: Await+Set => Await+FIN(Set)
    Finalize = function (me)
        if (not me[1]) or (me[1].tag ~= 'Stmts') then
            return      -- normal finalize
        end

        ASR(me[1][1].tag == 'AwaitInt', me,
            'invalid finalize (multiple scopes)')

        -- invert fin <=> await
        local ret = me[1]   -- return stmts
        me[1] = ret[2]      -- await => fin
        ret[2] = me         -- fin => stmts[2]
        return ret
    end,
---

--?
    SetBlock_pre = function (me)
        me.blk = _AST.iter'Block'()
    end,
    _Return = function (me)
        local set = _AST.iter'SetBlock'()
        local fr = unpack(me)
        --local to = _AST.node('Var')(me.ln,set[2][1])
        local to = _AST.copy(set[2])
        to.blk = set.blk
        to.ret = true

        local blk = _AST.node('Stmts')(me.ln)
        blk[#blk+1] = _AST.node('SetExp')(me.ln, '=', fr, to, set[3])

        blk[#blk+1] = _AST.node('Return')(me.ln)
        return blk
    end,

    _Continue = function (me)
        local _if  = _AST.iter('If')()
        local loop = _AST.iter('Loop')()
        ASR(_if and loop, me,
            'invalid `continue´')

        loop.hasContinue = true
        _if.hasContinue = true
        ASR( _if[3].tag=='Nothing'     and   -- no else
            me.depth  == _if.depth+3   and   -- If->Block->Stmts->Continue
             _if.depth == loop.blk.depth+2 , -- Block->Stmts->If
            me, 'invalid `continue´')
        return _AST.node('Nothing')(me.ln)
    end,
    Loop = function (me)
        if not me.hasContinue then
            return
        end

        -- start from last to first continue
        local stmts = me.blk[1]
        local N = #stmts
        local has = true
        while has do
            has = false
            for i=N, 1, -1 do
                local n = stmts[i]
                if n.hasContinue then
                    has = true
                    N = i-1
                    local _else = _AST.node('Stmts')(n.ln)
                    n[3] = _else
                    for j=i+1, #stmts do
                        _else[#_else+1] = stmts[j]
                        stmts[j] = nil
                    end
                end
            end
        end
    end,
}

_AST.visit(F)