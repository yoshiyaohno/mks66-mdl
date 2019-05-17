{
module Parser (parse) where

import Lexer

}

%name parse
%tokentype { Token }
%error { parseError }

%token
    str                 { TkString $$ }
    dbl                 { TkDouble $$ }
    light               { TkLight }
    constants           { TkConstants }
    save_coords         { TkSaveCoords }
    camera              { TkCamera }
    ambient             { TkAmbient }
    sphere              { TkSphere }
    torus               { TkTorus }
    box                 { TkBox }
    line                { TkLine }
    mesh                { TkMesh }
    texture             { TkTexture }
    set                 { TkSet }
    move                { TkMove }
    scale               { TkScale }
    rotate              { TkRotate }
    basename            { TkBasename }
    save_knobs          { TkSaveKnobs }
    tween               { TkTween }
    frames              { TkFrames }
    vary                { TkVary }
    push                { TkPush }
    pop                 { TkPop }
    save                { TkSave }
    generate_rayfiles   { TkGenerateRayfiles }
    shading             { TkShading }
    shading_type        { TkShadingType $$ }
    setknobs            { TkSetknobs }
    focal               { TkFocal }
    display             { TkDisplay }
    web                 { TkWeb }
    ':'                 { TkColon }

%%

Input   : Input Command     { $2:$1 }
        | {- empty -}       { [] }
        ;

Command : sphere dbl dbl dbl dbl
            { CmdSphere Nothing $2 $3 $4 $5 Nothing }
        | sphere str dbl dbl dbl dbl
            { CmdSphere (Just $2) $3 $4 $5 $6 Nothing }
        | sphere dbl dbl dbl dbl str
            { CmdSphere Nothing $2 $3 $4 $5 (Just $6) }
        | sphere str dbl dbl dbl dbl str
            { CmdSphere (Just $2) $3 $4 $5 $6 (Just $7) }

        | box dbl dbl dbl dbl dbl dbl
            { CmdBox Nothing $2 $3 $4 $5 $6 $7 Nothing }
        | box str dbl dbl dbl dbl dbl dbl
            { CmdBox (Just $2) $3 $4 $5 $6 $7 $8 Nothing }
        | box dbl dbl dbl dbl dbl dbl str
            { CmdBox Nothing $2 $3 $4 $5 $6 $7 (Just $8) }
        | box str dbl dbl dbl dbl dbl dbl str
            { CmdBox (Just $2) $3 $4 $5 $6 $7 $8 (Just $9) }

        | torus dbl dbl dbl dbl dbl
            { CmdTorus Nothing $2 $3 $4 $5 $6 Nothing }
        | torus str dbl dbl dbl dbl dbl
            { CmdTorus (Just $2) $3 $4 $5 $6 $7 Nothing }
        | torus dbl dbl dbl dbl dbl str
            { CmdTorus Nothing $2 $3 $4 $5 $6 (Just $7) }
        | torus str dbl dbl dbl dbl dbl str
            { CmdTorus (Just $2) $3 $4 $5 $6 $7 (Just $8) }

        | line dbl dbl dbl dbl dbl dbl
            { CmdLine Nothing $2 $3 $4 Nothing $5 $6 $7 Nothing }
        | line dbl dbl dbl str dbl dbl dbl
            { CmdLine Nothing $2 $3 $4 (Just $5) $6 $7 $8 Nothing }
        | line dbl dbl dbl dbl dbl dbl str 
            { CmdLine Nothing $2 $3 $4 Nothing $5 $6 $7 (Just $8) }
        | line dbl dbl dbl str dbl dbl dbl str
            { CmdLine Nothing $2 $3 $4 (Just $5) $6 $7 $8 (Just $9) }
        | line str dbl dbl dbl dbl dbl dbl
            { CmdLine (Just $2) $3 $4 $5 Nothing $6 $7 $8 Nothing }
        | line str dbl dbl dbl str dbl dbl dbl
            { CmdLine (Just $2) $3 $4 $5 (Just $6) $7 $8 $9 Nothing }
        | line str dbl dbl dbl dbl dbl dbl str
            { CmdLine (Just $2) $3 $4 $5 Nothing $6 $7 $8 (Just $9) }
        | line str dbl dbl dbl str dbl dbl dbl str
            { CmdLine (Just $2) $3 $4 $5 (Just $6) $7 $8 $9 (Just $10) }

        | mesh ':' str
            { CmdMesh Nothing $3 Nothing }
        | mesh str ':' str
            { CmdMesh (Just $2) $4 Nothing }
        | mesh ':' str str
            { CmdMesh Nothing $3 (Just $4) }
        | mesh str ':' str str
            { CmdMesh (Just $2) $4 (Just $5) }

        | move dbl dbl dbl
            { CmdMove $2 $3 $4 Nothing }
        | move dbl dbl dbl str
            { CmdMove $2 $3 $4 (Just $5) }

{

parseError :: [Token] -> a
parseError _ = error "parse error haha"

type Db = Double
type MS = Maybe String

data Command
        = CmdSphere MS Db Db Db Db MS
        | CmdBox MS Db Db Db Db Db Db MS
        | CmdTorus MS Db Db Db Db Db MS
        | CmdLine MS Db Db Db MS Db Db Db MS
        | CmdMesh MS String MS
        | CmdMove Db Db Db MS
        deriving (Eq, Show)

}
