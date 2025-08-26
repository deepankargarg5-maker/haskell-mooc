Set7.hs:155:21: error:
    Not in scope: type constructor or class ‘Add2’
    |
155 | instance Operation2 Add2 where
    |                     ^^^^

Set7.hs:156:13: error:
    Not in scope: data constructor ‘Add2’
    Perhaps you meant ‘Add1’ (line 133)
    |
156 |   compute2 (Add2 i j) = i + j
    |             ^^^^

Set7.hs:157:10: error:
    Not in scope: data constructor ‘Add2’
    Perhaps you meant ‘Add1’ (line 133)
    |
157 |   show2 (Add2 i j) = show i ++ "+" ++ show j
    |          ^^^^

Set7.hs:159:21: error:
    Not in scope: type constructor or class ‘Subtract2’
    |
159 | instance Operation2 Subtract2 where
    |                     ^^^^^^^^^

Set7.hs:160:13: error:
    Not in scope: data constructor ‘Subtract2’
    Perhaps you meant one of these:
      ‘Subtract1’ (line 134), variable ‘subtract’ (imported from Prelude)
    |
160 |   compute2 (Subtract2 i j) = i - j
    |             ^^^^^^^^^

Set7.hs:161:10: error:
    Not in scope: data constructor ‘Subtract2’
    Perhaps you meant one of these:
      ‘Subtract1’ (line 134), variable ‘subtract’ (imported from Prelude)
    |
161 |   show2 (Subtract2 i j) = show i ++ "-" ++ show j
    |          ^^^^^^^^^
