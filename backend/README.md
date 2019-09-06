# Simple User backend

A simple user backend resembling a CRUD, it uses haskell's [TVar] in order to share values between clients without setting up a database.

Only drawback would be that all data is lost when the server reboots.

All exposed endpoints are based the following ADT:
``` haskell
data User = User
  { uId      :: !Natural
  , uName    :: !Text
  , uAge     :: !Natural
  , uHobbies :: ![Hobby]
  }
```
[TVar]:(https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Conc-Sync.html#t:TVar)
