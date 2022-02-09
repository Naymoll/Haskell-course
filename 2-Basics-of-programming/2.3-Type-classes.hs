class Printable t where
    toString :: t -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"