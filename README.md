# LessPass
Haskell Implementation of [LessPass](https://github.com/lesspass/lesspass) Command Line Interface

LessPass is a stateless password manager.

Stop wasting your time synchronizing your encrypted vault.
Remember one master password to access your passwords, anywhere, anytime.
No sync needed. Try the demo at https://www.lesspass.com.

## Supported Flags
| Flag | Description |
|------|-------------|
| -C   | Counter     |
| -L   | Password Length |
| -l / --lowercase | Enable Lowercase characters |
| -u / --uppercase | Enable Uppercase characters |
| -d / --digits    | Enable Digits |
| -s / --symbols   | Enable Symbols |

## Unsupported Flags
This implementation doesn't support fingerprinting,
so there are no emojis to show that you entered your
master password correctly.

| Flag | Description |
|------|-------------|
| --no* | No symbols, No lowercase, etc. Use the positive supported flags |
| --url | LessPass database URL |
| save/load | Flags to save/load from LessPass database |
| exclude | Exclude specific characters from the output |
| -c | Copy to clipboard |

## Dependencies
- OpenSSL - Uses the OpenSSL Library for PBKDF2 implementation
