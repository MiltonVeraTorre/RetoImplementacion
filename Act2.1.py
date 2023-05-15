#	Codigo para la gramatica de expresiones vista en clase
#	en version no-ambigua 
'''
Eliezer Cavazos Rochin / A00835194
Marco Ottavio Podesta Vezzali / A00833604
Milton Vera De la Torre / A00833471
Aldo Bazarra Armendáriz / A00833993
Franco	Enrique Lugo Meza / A00833951
'''
#	F –> ( E ) | id		
#	E -> T E'
#	E' -> + T E' | epsilon
#	E' -> - T E' | epsilon
#	T –> F T' 				
#	T' –> * F T' | epsilon
#	T' –> / F T' | epsilon
# 	I -> If F J I | epsilon
#	I -> Else F J I | epsilon
#	I -> id F| epsilon
#	I -> J
#	J -> : | epsilon



def sigToken():
	global i
	i = i + 1
	print("current token es ", linea[i])
	return linea[i]

def showError(expected, token):
	print("ERROR: esperaba ", expected, ", recibio ", token)
	global stop 
	stop = True

#	Factores: simbolo no-terminal F
#	F –> ( E ) | id			
def es_F():
	global token
	global last
	last = "( or id"
	if token == "(":							#	veremos si es (E)
		print("\t( ok")
		token = sigToken()						
		
		es_E() 									#  veremos si es E
		
		last = ")"									
		if token == ")":							
			print("\t) ok")
			token = sigToken()
			
		else:
			showError("\t)", token)
		
	elif token == "id":
		print("\tid ok")
		token = sigToken()
	else:
		showError("(E) or id", token)
		
#	Expresiones: simbolo no-terminal E
#	E -> T E'
def es_E():
	global token
	global stop
	if stop == False:
		es_T()
		es_Eprime()

#	E' -> + T E' | epsilon
def es_Eprime():
	global token
	global last
	last = "operador"
	if token == "+":
		print("\t+ ok" )
		token = sigToken()		
		
		es_T()
		es_Eprime()
	elif token == "-":
		print("\t- ok" )
		token = sigToken()		
		
		es_T()
		es_Eprime()
	# si no es +, no habra mas recursion
	

#	Terminos: simbolo no-terminal T
# T –> F T' 				
# T' –> * F T' | epsilon	
def es_T(): 
	global token
	es_I()
	es_Tprime()
			

def es_Tprime():
	global token
	global last
	last = "operador"
	if token == "*":
		print ("\t* ok")
		token = sigToken()
		
		es_I()
		es_Tprime()
	if token == "/":
		print ("\t/ ok")
		token = sigToken()
		
		es_I()
		es_Tprime()
	#	si no es *, no habra mas recursion


def es_I():
	global token
	global last
	last = "(E) o id o condicional "
	if token == "if":
		print ("\tif statement ok")
		token = sigToken()
		
		es_F()
		es_J()
		es_I()

	elif token == "else":
		print ("\telse statement ok")
		token = sigToken()
		
		es_F()
		es_J()
		es_I()

	#	si no es *, no habra mas recursion
	elif token == "id" or token == "(" :
		es_F()
		#es_I()

def es_J():
	global token
	global last
	last = "dos puntos"
	if token == ":":
		print("\t: statement ok")
		token = sigToken()
		es_F()
	else:
		showError(":", token)


	

#	cada linea tiene un vacio al final, para evitar desbordar
lineas = [	["id", "+", "(", "id", "*", "id", ")", ""]
 			,["id", "+", "id", ""]
 			,["id", "*", "id", ""]
 			,["(","id",")", "+", "(", "id", "*", "id",")", "" ]
 			,["id", "-", "id", "*", "id", ""]
 			,["id", "/", "id", "*", "id", ""]
 			,["id", "*", "id", "id", ""]												#	nope
 			,["id", "id", ""]															#	nope	
			,["if", "id", ""]															#	nope
			,["else", "id", ""]															#	nope
			,["if", "id", "*", ":", ""]													#	nope	
			,["if", "(", "id", ")", ":", "id", "*", "id", "else", "id", "*", "id"]		#	nope
			,["if", "(", "id", ")", ":", "id", "*", "id", ""]
 			,["(", "id", ")", "+", "(", "id", ")", ""]	# oks
			,["if", "(", "id","*","id", ")", ":", "id", "else", "(", "id","+","id", ")", ":", "id", ""]
			,["(", "id", ")", ""]
 		]

lex = ["+", "*", "(", ")", "id", "", "-", "/"]	# estos son los simbolos terminales, i.e., lexemas validos

for linea in lineas: 
	global stop
	stop = False
	print("\n",linea)

	i = -1
	token = sigToken()
	
	es_E()					#	Revisa si la linea es una expresion valida

	if token == "" and stop == False:			#	si consumio toda la linea, esta bien
		print("OKS\n")
	else:
		showError(last, token)
		print("NOPE\n")