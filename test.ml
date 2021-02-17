#use "main.ml";;

let e = 
    Let("Magazzino", 
        Dictionary
            (Assoc("mele", CstInt(430), 
                Assoc("banane", CstInt(312), 
                    Assoc("arance", CstInt(525),
                        Assoc("pere", CstInt(217), Empty))))),
    InsertDict("kiwi", CstInt(300), Den("Magazzino")));;

let e2 = 
    Let("Magazzino",
        Dictionary
            (Assoc("mele", CstInt(430), 
                Assoc("banane", CstInt(312), 
                    Assoc("arance", CstInt(525), 
                        Assoc("pere", CstInt(217), Empty))))),
    DeleteDict("pere", Den("Magazzino")));;

let e3 = 
    Let("Magazzino", 
        Dictionary
            (Assoc("mele", CstInt(430), 
                Assoc("banane", CstInt(312), 
                    Assoc("arance", CstInt(525), 
                        Assoc("pere", CstInt(217), Empty))))),
    HasKey("banane", Den("Magazzino")));;

let e4 = 
    Let("Magazzino", 
        Dictionary(Assoc("mele", CstInt(430), 
            Assoc("banane", CstInt(312), 
                Assoc("arance", CstInt(525), 
                    Assoc("pere", CstInt(217), Empty))))),
    Iterate(Fun("x", Sum(Den("x"), CstInt(1))), Den("Magazzino")));;

let e5 = 
    Let("Magazzino", 
		Dictionary
			(Assoc("mele", CstInt(430), 
				Assoc("banane", CstInt(312), 
					Assoc("arance", CstInt(525), 
						Assoc("pere", CstInt(217), Empty))))),
		Fold
			(FunArgs(
				Ide("x", Ide("y", Empty)), 
				Sum(Sum(Den("x"), Den("y")), CstInt(1))), 
			Den("Magazzino")));;

let e6 = 
    Let("Magazzino", 
            Dictionary
				(Assoc("mele", CstInt(430), 
					Assoc("banane", CstInt(312), 
						Assoc("arance", CstInt(525), 
							Assoc("pere", CstInt(217), Empty))))),
            Filter(
                Ide("mele", Ide("pere", Empty)),
                Den("Magazzino")));;




let e = 
    Let("Magazzino", 
        Dictionary
            (Assoc("mele", CstInt(430), 
                Assoc("banane", CstInt(312), 
                    Assoc("arance", CstInt(525),
                        Assoc("pere", CstInt(217), Empty))))),
    InsertDict("kiwi", CstString("300"), Den("Magazzino")));;   (* Il dizionario supporta anche l'inserimento di tipi diversi *)



let e = 
    Let("Magazzino", 
        Dictionary
            (Assoc("mele", CstInt(430), 
                Assoc("banane", CstInt(312), 
                    Assoc("arance", CstInt(525),
                        Assoc("pere", CstInt(217), Empty))))),
    DeleteDict("kiwi", Den("Magazzino")));;   (* Passare a DeleteDict una chiave non presente produce un errore Unexisting key *)

(* eval e (emptyenv(String("Unbound value")));; *)


let e = 
    Let("Magazzino", 
		Dictionary
			(Assoc("mele", CstInt(430), 
				Assoc("banane", CstInt(312), 
					Assoc("arance", CstInt(525), 
						Assoc("pere", CstInt(217), Empty))))),
		Fold
			(FunArgs(
				Ide("x", Ide("y", Ide("z", Empty))), 				(* Passare a Fold una funzione che prende un numero di parametri diverso da 2 produce un'eccezione *)
				Sum(Sum(Den("x"), Den("y")), CstInt(1))), 
			Den("Magazzino")));;

(* eval e (emptyenv(String("Unbound value")));; *)
