# dired-k.el

`dired-k.el` highlights dired buffer like [k](https://github.com/supercrabtree/k).

(This may works only default dired setting)


## screenshot

![dired-k](image/dired-k.png)


## Commands

### `dired-k`

Highlight dired buffer by following parameters.

- File size
- Modified time
- Git status(if here is in git repository)


## Sample Configuration

```lisp
(require 'dired-k)
(define-key dired-mode-map (kbd "K") 'dired-k)
```
