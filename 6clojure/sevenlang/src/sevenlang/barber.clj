; Barber Shop
;  Waiting Room - 3 Chairs
;  Barber Chair - 1 Chair, 1 Barber
;  Customer's arrive every 10-30 ms
;  If waiting room is full, customer is turned away
;  Each haircut takes 20 ms
;  When the barber chair is empty, a customer from the waiting room gets a haircut
; How many customers are serviced in 10 s?


; Can represent chairs as an int - 
;    0 = no customers waiting ---> 3 = waiting room full
; Barber can be a boolean reference (true if customer getting haircut, false otherwise)
;    getHaircut -> subtracts from the waiting room, sets Barber to true, and start a future
;                  Thread/sleep 20 that then sets Barber to false and asks for a new customer
; New customer starts a future that after 10-30 ms add a new customer to the waiting room
;    and recur 
; When a new customer is added to the waiting room, if waiting room is full (=3) do nothing,
;    otherwise increment waiting room, and check barber chair availability 


(defn create
    "Initialize the Barber Shop"
    []
    (def waiting (atom 0))
    (def barber (atom false))
    (def customers (atom 0)))

(declare new_haircut done_haircut generate_customer barber_sim)

(defn new_customer
    "Add a new customer to the barber shop"
    []
    ; Only accept a customer if we don't have 3 customers
    (if (< @waiting 3) 
        (do 
            (swap! waiting inc)
            (new_haircut))))

(defn new_haircut
    "Check if the barber is free and if there are waiting customers, if so start a new haircut"
    []
    (if (and (> @waiting 0) (not @barber))
        (do 
            (swap! waiting dec)
            (reset! barber true)
            (future (Thread/sleep 20) (done_haircut))
            )))

(defn done_haircut
    "When haircut is done, customer leaves barber chair and a new one takes his place"
    []
    ; Kick customer out of barber chair and check for new customer
    (reset! barber false)
    ; Increment number of satisfied custoemrs
    (swap! customers inc)
    (new_haircut))

(defn start 
    "Start the barber shop simulation to run for x milliseconds"
    [x]
    ; running will be false when running completes (used to stop the generate 
    ; customers thread) and total customers is an agent that the final number
    ; of satisfied customers will be stored in
    (def running (atom true))
    (def total_customers (agent 0))

    ; Initialize all atoms and begin the simulation, after completion return customer count
    (println (str "Running barber shop simulation for " (/ x 1000) " seconds..."))
    (create)
    (send total_customers barber_sim x)
    (await total_customers)
    (println (str "The barber has completed " @total_customers " haircuts")))

(defn barber_sim
    "Simulate the barbershop for x milliseconds"
    [c x]
    ; Start generating customers
    (generate_customer)

    ; Wait for the simulation to run for x ms
    (Thread/sleep x)

    ; Running is complete, return customer count
    (swap! running not)
    @customers)

(defn generate_customer
    "Generates a new customer after 10-30 ms"
    []
    ; Only continue generating customers if the simulation is running
    (if @running 
    (future (Thread/sleep (+ (rand-int 20) 10)) (do 
        (new_customer)
        (generate_customer)))))

(start 10000)