#encrypting data
# x <- serialize(list(1,2,3), NULL)
# 
# passphrase <- charToRaw("This is super secret")
# key <- sha256(passphrase)
# 
# encrypted_x <- aes_cbc_encrypt(x, key = key)
# 
# saveRDS(encrypted_x, "secret-x.rds")
# 
# encrypted_y <- readRDS("secret-x.rds")
# 
# y <- unserialize(aes_cbc_decrypt(encrypted_y, key = key))
# 
# 
#
passphrase <- charToRaw("My passphrase")
key <- sha256(passphrase)

#west_coast_fatal
x <- serialize(fread("west_coast_fatal_state.csv"), NULL)
encrypted_x <- aes_cbc_encrypt(x, key = key)
saveRDS(encrypted_x, "west_coast_fatal_state.rds")

#non_fatal_event
x <- serialize(fread("west_coast_non_fatal_event.csv"), NULL)
encrypted_x <- aes_cbc_encrypt(x, key = key)
saveRDS(encrypted_x, "west_coast_non_fatal_event.rds")

#non_fatal_source
x <- serialize(fread("west_coast_non_fatal_source.csv"), NULL)
encrypted_x <- aes_cbc_encrypt(x, key = key)
saveRDS(encrypted_x, "west_coast_non_fatal_source.rds")

#non_fatal_state
x <- serialize(fread("west_coast_non_fatal_state.csv"), NULL)
encrypted_x <- aes_cbc_encrypt(x, key = key)
saveRDS(encrypted_x, "west_coast_non_fatal_state.rds")




