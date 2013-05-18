module ASTBuildUtils

open Tree

let markers (lpos:Pos) (rpos:Pos) = Pos(lpos.StartPos, rpos.EndPos)

(*annotate using markers*)
let tbb item lpos rpos = TopDeclA(item, markers lpos rpos)
let tba item lpos (rightA:Annot)          = tbb item lpos rightA.Pos
let tab item (leftA:Annot) rpos           = tbb item leftA.Pos rpos
let taa item (leftA:Annot) (rightA:Annot) = tbb item leftA.Pos rightA.Pos

let nbb item lpos rpos = NamespaceDeclA(item, markers lpos rpos)
let nba item lpos (rightA:Annot)          = nbb item lpos rightA.Pos
let nab item (leftA:Annot) rpos           = nbb item leftA.Pos rpos
let naa item (leftA:Annot) (rightA:Annot) = nbb item leftA.Pos rightA.Pos

let cbb item lpos rpos = ClassDeclA(item, markers lpos rpos)
let cba item lpos (rightA:Annot)          = cbb item lpos rightA.Pos
let cab item (leftA:Annot) rpos           = cbb item leftA.Pos rpos
let caa item (leftA:Annot) (rightA:Annot) = cbb item leftA.Pos rightA.Pos

let ibb item lpos rpos = InterfaceDeclA(item, markers lpos rpos)
let iba item lpos (rightA:Annot)          = ibb item lpos rightA.Pos
let iab item (leftA:Annot) rpos           = ibb item leftA.Pos rpos
let iaa item (leftA:Annot) (rightA:Annot) = ibb item leftA.Pos rightA.Pos

let ebb item lpos rpos = ExprA(item, markers lpos rpos)
let eba item lpos (rightA:Annot)          = ebb item lpos rightA.Pos
let eab item (leftA:Annot) rpos           = ebb item leftA.Pos rpos
let eaa item (leftA:Annot) (rightA:Annot) = ebb item leftA.Pos rightA.Pos

let ea item pos = ExprA(item, pos)

let eanp item = ea item Pos.NilPos
let ianp item = InterfaceDeclA (item, Pos.NilPos)
let canp item = ClassDeclA (item, Pos.NilPos)
let nanp item = NamespaceDeclA (item, Pos.NilPos)