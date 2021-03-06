-- Embedded map of IPA symbol attributes
{-# LANGUAGE QuasiQuotes #-}

module IpaMap (ipaMap) where

import Text.RawString.QQ

-- The map has format: IPA:CV:IPA'
-- where IPA is the original IPA, CV is either C[onsonant] or V[owel],
-- IPA' is the unified IPA used for rhyming.

ipaMap = [r|# Please populate uncategorized entries
ç:C:h
nʲ:C:n
ɑ̃::V:a
ɔ̃:V:o
ɬ:C:j
b:C:b
eɪ:V:e
h:C:h
i:V:i
k:C:k
l:C:l
oʊ:V:o
p:C:p
s:C:s
t:C:t
v:C:v
ɑ:V:a
ə:V:e
ɚ:V:u
ɜ:V:e
ɪ:V:i
ɹ:C:r
ɾ:C:t
ʒ:C:3
dʒ:C:3
m:C:m
ɔ:V:o
ɔɹ:V:o
aɪ:V:a
d:C:d
f:C:f
j:C:j
n:C:n
oɹ:V:o
tʃ:C:4
u:V:u
w:C:w
z:C:z
æ:V:e
ð:C:5
ŋ:C:n
ɐ:V:a
ɑɹ:V:a
əl:V:a
ɛ:V:e
ɛɹ:V:e
ɡ:C:g
ʃ:C:4
ʊ:V:u
ʊɹ:V:u
ʌ:V:a
θ:C:5
ᵻ:V:i
aɪə:V:a
aɪɚ:V:a
aʊ:V:o
iə:V:i
n̩:C:n
o:V:o
ɔɪ:V:o
ɪɹ:V:i
ʔ:C:9
a:V:a
aɪl:V:e
e:V:e
ndʒ:C:n
q:C:k
r:C:r
sk:C:s
sɛ:C:s
æɪ:V:e
ɚl:C:l
ɲ:C:n
ɹuɛ:V:u
aʊə:V:a
eə:V:e
ɒ:V:a
əʊ:V:e
ʊə:V:u
a:V:a
əɹ:V:e
ʉ:V:u
ʉɹ:V:u
ʌɹ:V:a
ʌʉ:V:a
bʲ:C:b
dʲ:C:d
fʲ:C:f
ja:V:a
ju:V:u
kʲ:C:k
mʲ:C:m
pʲ:C:p
rʲ:C:r
sʲ:C:s
ts:C:t
tʃʲ:C:s
tʲ:C:t
u":V:u
vʲ:C:v
x:C:s
y:V:i
ɕ:C:8
ɡʲ:C:g
ɪ^:C:j
ɭ:C:l
ɵ:V:a
ʑ:C:z
aɪʊɹ:V:a
ɫ:C:l
|]
