# noir-ts-mode
[![MELPA](https://melpa.org/packages/noir-ts-mode-badge.svg)](https://melpa.org/#/noir-ts-mode)
[![MELPA Stable](https://stable.melpa.org/packages/noir-ts-mode-badge.svg)](https://stable.melpa.org/#/noir-ts-mode)

An Emacs tree-sitter mode for the Noir Language.

## Installation

1. Make sure you have Emacs version 29.x and above.

2. Add the grammar to your settings as so:
``` elisp
(add-to-list
 'treesit-language-source-alist
 '(noir "https://github.com/hhamud/tree-sitter-noir" "main" "src"))
```

3. Install the grammar
``` elisp
M-x treesit-install-language-grammar RET noir RET
```

4. Check if it has installed successfully
``` elisp
(treesit-language-available-p 'noir)
```

5. Download `noir-ts-mode.el` from the [GitHub repository](https://github.com/hhamud/noir-ts-mode).

6. Move `noir-ts-mode.el` to a directory in your Emacs load path.

7. Add the following code to your Emacs configuration file:

```elisp
(require 'noir-ts-mode)
```

## Usage

Once noir-ts-mode.el is installed, it will automatically be enabled when you open a .nr file. The major mode will provide syntax highlighting for editing Noir code.

## Testing

Run the ERT suite in batch Emacs with:

```shell
emacs --batch -Q -L . -L test -l test/noir-ts-mode-tests.el -f noir-ts-mode-test-run
```

## Contributing

Contributions to noir-ts-mode are welcome. If you find any issues or have suggestions for improvement, please create a new issue or submit a pull request on the [GitHub repository](https://github.com/hhamud/noir-ts-mode).
