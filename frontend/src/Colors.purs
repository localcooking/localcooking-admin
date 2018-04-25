module Colors where

import MaterialUI.MuiThemeProvider (ColorPalette)


palette :: {primary :: ColorPalette, secondary :: ColorPalette}
palette =
  { primary:
    { light: "#819ca9"
    , main: "#546e7a"
    , dark: "#29434e"
    , contrastText: "#ffffff"
    }
  , secondary:
    { light: "#b6ffff"
    , main: "#81d4fa"
    , dark: "#4ba3c7"
    , contrastText: "#000000"
    }
  }
