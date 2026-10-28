# Bundled fonts

`SymbolsNerdFontMono-Regular.ttf` is Symbols Nerd Font Mono from the
[Nerd Fonts](https://github.com/ryanoasis/nerd-fonts) release v3.4.0
(`NerdFontsSymbolsOnly.zip`, SHA-256
`8e617904b980fe3648a4b116808788fe50c99d2d495376cb7c0badbd8a564c47`). It holds
the icons that programs such as Emacs, Neovim and shell prompts print from
Unicode's private use area: Powerline, Font Awesome, Material Design,
Codicons, Devicons, Octicons and others. Its family is first in
`*font-fallback-families*`, so slt draws icons the chosen font lacks with
it, and they work without a patched font installed, as in Ghostty (which
embeds the same font). The Mono variant keeps each icon one cell wide.
The build copies this directory next to the executable; on macOS slt
registers its fonts for the process with CoreText.

Nerd Fonts is under the MIT license (`LICENSE-SymbolsNerdFont`); the
icon sets it collects keep their own licenses, listed in the Nerd Fonts
repository's `license-audit.md`.
