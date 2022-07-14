
# security functions ------------------------------------------------------

# method
dolock <- function(obj, key) UseMethod("dolock")
unlock <- function(obj, key) UseMethod("unlock")

# default
dolock.default <- function(obj, key) aes_cbc_encrypt(serialize(obj, NULL), key = sha256(charToRaw(key)))
unlock.default <- function(obj, key) unserialize(aes_cbc_decrypt(obj, key = sha256(charToRaw(key))))

# list
dolock.list <- function(obj, key) lapply(obj, function(x) dolock(x, key))
unlock.list <- function(obj, key) lapply(obj, function(x) unlock(x, key))
