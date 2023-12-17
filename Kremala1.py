import random
import sys

def printguessed(letter, b):
    if letter in b:
        print(" {} ".format(letter), end="")
    else:
        print(" _ ", end="")
        
def notfound(x,c,z,b):
    if x.upper() in c:
        print('Εχετε ήδη δοκιμάσει το γράμμα ',x.upper())
    else:
        print('Το γράμμα', x.upper(),  'δεν περιέχεται στην λέξη!')
        z=z-1
        c+=x.upper()
        str = ', '
        print('Εχετε δοκιμάσει ήδη τα γράμματα: ' + str.join(b+c))
        print('Σας απομένουν', z,  'προσπαθειες')
    return z
        
def found(x,b,a):
    if x.upper() in b:
        print('Εχετε ήδη μαντέψει το γράμμα ',x.upper())
    else:
        print('Το γράμμα', x.upper(),  'περιέχεται στην λέξη!')
        b += x.upper()
        if len(b) == len("".join(set(a))):
            print(a)
            print('Κερδίσατε!')
            sys.exit()
        
def importcheck(x,a,b,z,c):
    if len(x) > 1 or not x.isalpha():
        print('Λάθος εισαγωγή')
        sys.exit()
    else:
        if x.upper() in a :
            found(x,b,a)
        else:
            z=notfound(x,c,z,b)
        return z
            
def kremala():
    a = ['ΣΧΟΛΕΙΟ', 'ΚΑΦΕΝΕΙΟ', 'ΠΑΝΕΠΙΣΤΗΜΙΟ', 'ΔΑΣΚΑΛΟΣ', 'ΜΑΘΗΤΗΣ', 'ΕΞΕΤΑΣΗ', 'ΜΑΘΗΜΑ'
         , 'ΕΡΓΑΣΙΑ', 'ΕΤΑΙΡΙΑ']
    b = []
    c = []
    z = 6
    a = random.choice(a)
    print('Mπορείτε να βρείτε την λέξη ?\n')

    print(" _ " * len(a))
    print('\nΣας απομένουν', z,  'προσπαθειες')
    while z>0:
        x = input('\n\nΠαρακαλώ δώστε ένα γράμμα:')
        z=importcheck(x,a,b,z,c)
                
        for letter in a:
                printguessed(letter,b)
        print("\n")
        
    if z == 0:
        print('Δυστυχώς χάσατε!')
        print('Η λέξη που έπρεπε να βρεθεί ηταν η', a)
        
kremala()
