(defn create
    []
    (def accounts (ref [])))

(defn num_accounts
    []
    (count @accounts))

; TODO: Error check using num_accounts to not access out of bounds
; TODO: Check balance of account before withdrawing
; TODO: Ensure x is positive
(defn credit
    "Take x money from account b and give it to account a"
    [a b x]
    (dosync (alter accounts assoc a (+ (nth @accounts a) x)))
    (dosync (alter accounts assoc b (- (nth @accounts b) x))))

; TODO: Error check using num_accounts to not access out of bounds
; TODO: Check balance of account before withdrawing
; TODO: Ensure x is postiive
(defn debit
    "Take x money from account a and give it to account b"
    [a b x]
    (dosync (alter accounts assoc a (- (nth @accounts a) x)))
    (dosync (alter accounts assoc b (+ (nth @accounts b) x))))

; TODO: Ensure balance is positive
(defn add-account
    "Add a new account with given initial balance"
    [balance]
    (dosync (alter accounts conj balance)))

(defn printAccounts 
    "Print current balance of all accounts"
    []
    (println @accounts))

; Run some test functions
(create)

; Add some accounts
(add-account 100)
(add-account 200)
(add-account 150)
(printAccounts)

; Do some account modification
(credit 0 1 50)
(credit 0 2 50)
(debit 0 1 100)
(printAccounts)

