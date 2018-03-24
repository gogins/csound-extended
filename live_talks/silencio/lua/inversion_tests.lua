Silencio = require("Silencio")
ChordSpace = require("ChordSpace")

local original = ChordSpace.chordsForNames['CM']
print(original:information())
print()

function invariantPcs(a_, b_) 
    local a = a_:eOP()
    local b = b_:eOP()
    local count = 0
    for voice = 1, #a do
        local p = a[voice]
        if a:count(p) == b:count(p) then
            count = count + 1
        end
    end
    return count
end

function J(chord, n, g, i)
    g = g or 1
    local inversions = {}
    local index = 0
    for I = 1, 11, g do
        local inversion = chord:I(I)
        if invariantPcs(chord, inversion) == n then
            index = index + 1
            table.insert(inversions, inversion:eOP())
        end
    end
    local result = ChordSpace.sortedSet(inversions)
    if i ~= nil then
        if i > #result then
            return nil
        else
            return result[i]
        end
    end
    return result
end

print("Inversions of CM")
for i=0, 12 do
    local inversion = original:I(i):eOP()
    print(i, inversion, invariantPcs(original, inversion))
end
print()
print("J(1)")
local J1 = J(original, 1)
for i, j in ipairs(J1) do
    print(i, j)
end
print()
print("J(2)")
local J2 = J(original, 2)
for i, j in ipairs(J2) do
    print(i, j)
end
print()
print("J(2, 1, 2)")
local J212 = J(original, 2, 1, 2)
print(J212)
print()

