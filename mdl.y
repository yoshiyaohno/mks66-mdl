{
module Parser (parse) where
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    string              { TkString $$ }
    double              { TkDouble $$ }
    light               { TkLight }
    constants           { TkConstants }
    save_coords         { TkSaveCoords }
    camera              { TkCamera }
    ambient             { TkAmbient }
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
    shading_type        { TkShadingtype $$ }
    setknobs            { TkSetknobs }
    focal               { TkFocal }
    display             { TkDisplay }
    web                 { TkWeb }
    ':'                 { TkColon }

%%

Input   : Input Command     { $2:$1 }
        | {- empty -}       { [] }
        ;

Command : sphere double double double double
            { Sphere Nothing $2 $3 $4 $5 Nothing }
        | sphere string double double double double
            { Sphere $2 $3 $4 $5 $6 Norhing }
        | sphere double double double double string
            { Sphere Nothing $2 $3 $4 $5 $6 }
        | sphere string double double double double string
            { Sphere $2 $3 $4 $5 $6 $7 }

        | box double double double double
            { box Nothing $2 $3 $4 $5 Nothing }
        | box string double double double double
            { box $2 $3 $4 $5 $6 Norhing }
        | box double double double double string
            { box Nothing $2 $3 $4 $5 $6 }
        | box string double double double double string
            { box $2 $3 $4 $5 $6 $7 }

{
}
