#Εισαγωγή των βιβιλοθηκών που χριεαζόμαστε

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d

#Δημιουργία του data frame 

data=pd.read_csv(r"C:\Users\ppetr\Downloads\weather_data.csv",header=0)

df=pd.DataFrame(data)

# Ρυθμίζουμε το πρόγραμμα να δείχνει ολες τις γραμμές/στήλες

pd.set_option('display.max_columns',None)
pd.set_option('display.max_rows',None)

# Check ότι το dataframe έχει περάσει σωστά 

df.head()

# Εφόσον δουλεύουμε στο spyder, μπορούμε οποιαδήποτε στιγμη να ελέγξουμε το dataframe
# κάνοντας διπλό click πάνω του στον variable explorer 

# 1. Στις μέγιστες και ελάχιστες θερμοκρασίες υπάρχουν κάποιες ημέρες που δεν είχαμε 
# παρατηρήσεις (ΝaΝ). Χρησιμοποιώντας κυβική παρεμβολή των τριών πριν και τριών 
# μετά τιμών να προσεγγιστούν οι τιμές αυτές. Επίσης, τα κενά που υπάρχουν στην 
# στήλη MONTH να αντικατασταθούν με τους χαρακτήρες DEC.

# Λύση :

# Χρησιμοποιούμε την εντολή df.isna().sum() για να δούμε πόσα nan έχουμε ανα στήλη

print(df.isna().sum())

# Παρατηρούμε ότι δεν υπάρχουν NaN κελιά 
# Ελέγχουμε τις μοναδικές αξίες για τις συγκεκριμένες στήλες για να δουμε πώς απεικονίζονται 
# τα προβληματικά κελιά

for column in df.loc[:,['MONTH','HIGH','LOW']]:
    unique_values = df[column].unique()
    print(f"Mοναδικές τιμές στην στήλη {column}: {unique_values}")
    
# Παρατηρούμε ότι τα nan κελιά απεικονίζονται είτε με ' ' (κενό) είτε με '  ' (διπλό κενό). 
# Αλλάζουμε τις συγκεκριμένες τιμές σε NaN

df.replace([' ', '  '], pd.NA, inplace=True)

#Ξαναελέγχουμε τις στήλες για NaN τιμές

print(df.isna().sum())

#Αντικαθιστολυμε τα NaN κελιά στην στήλη ΜΟΝΤΗ με το DEC

df['MONTH'].fillna('DEC', inplace=True)

# Ελέγχουμε και βλέπουμε ότι οι τιμές έχουν περαστεί στην στήλη ΜΟΝΤΗ

df.iloc[:,0]

# Για να συμπληρώσουμε τις κενές τιμές στις στήλες HIGH και LOW δημιουργούμε 
# μια συνάρτηση που εφαρμόζει την interp1d στις τρείς προηγούμενες και τρείς 
# επόμενες τιμες τους

def cubic_interpolation(column):
    missing_values = column.index[column.isna()]
    for index in missing_values:
        if index >= 3 and index <= len(column) - 4:
            values_before = column[index - 3 : index]
            values_after = column[index + 1 : index + 4]
            
            # Δημιουργούμε τα Χ και Υ για την παρεμβολή
            x = np.concatenate([np.arange(index - 3, index), np.arange(index + 1, index + 4)])
            y = np.concatenate([values_before.dropna(), values_after.dropna()])
            
            
            # Eφαρμόζουμε την παρεμβολή
            ci = interp1d(x, y, kind='cubic')
            y_ci =  np.round(ci(index),1)
            
            # Επιστρέφουμε την αξία της παρεμβολής στην κενή τιμή της στήλης
            column[index] = y_ci
    return column

#Εφαρμόζουμε την εξίσωση στις στήλες HIGH και LOW

cubic_interpolation(df['HIGH'])
cubic_interpolation(df['LOW'])

# Ελέγχουμε και βλέπουμε ότι οι τιμές έχουν περαστεί στις στήλες HIGH kai LOW

print(df.isna().sum())
df.loc[:,['HIGH','LOW']]

# 2. Να προστεθεί στο τέλος του πίνακα μια γραμμή που θα υπολογίζει στην στήλη με 
# τις μέγιστες θερμοκρασίες την απόλυτα μέγιστη θερμοκρασία, στην στήλη με τις ελάχιστές 
# την απόλυτα ελάχιστη, και παρόμοια για την στήλη με την ισχυρότερη (wind high) ένταση ανέμου. 
# Στην περίπτωση της στήλης των μέσων θερμοκρασιών, θα βάζει την μέση τιμή τους ενώ θα αθροίζει 
# τα εκατοστά βροχόπτωσης, τις βαθμοημέρες θέρμανσης (HDD Heating Degree Days) και βαθμοημέρες 
# ψύξης (CDD Cooling Degree Days) στις αντίστοιχες στήλες. Τα υπόλοιπα θα παραμείνουν κενά.

# Λύση :
    
# Eλέγχουμε τους τυπους δεδομένων για τις στήλες του data frame 

df.dtypes

# Παρατηρούμε ότι στις στήλες HIGH και LOW έχουμε object type αντί για float
# Μετατρέπουμε τις δύο στήλες σε float data type

df['HIGH'] = df['HIGH'].astype(float)
df['LOW'] = df['LOW'].astype(float)
    
# Φτιάχνουμε μια καινούργια γραμμή με τα μεγέθη που ζητήθηκαν :
    
df.loc['Επισκόπηση'] = [np.nan, np.nan, df['TEMP'].mean(),
df['HIGH'].max(), np.nan, df['LOW'].min(), np.nan, df['HDD'].sum(), df['CDD'].sum(), df['RAIN'].sum(), np.nan,
df['WINDHIGH'].max(), np.nan, np.nan]

# 3. Να βρεθεί η διάμεσος και η τυπική απόκλιση των μέσων θερμοκρασιών.

# Λύση :
    
# Υπολογισμός των δύο μεγεθών :
    
median = df["TEMP"][:-1].median()
std = df["TEMP"][:-1].std()
    
print(f'Η διάμεσος των μέσων θερμοκρασιών είναι {median}')
print(f'Η τυπική απόκλιση των μέσων θερμοκρασιών είναι {std}')

# 4. Να εμφανιστούν πόσες μέρες φυσούσε αέρας από κάθε μία από τις πιθανές 
# διευθύνσεις. Να γίνει γράφημα πίττας που θα απεικονίζει την προηγούμενη 
# κατανομή των ημερών σε διεύθυνση ανέμου που φυσούσε.
    
# Λύση :
    
# Μετράμε τις μέρες με συγκεκριμένο αέρα 

df.groupby('DIR').size()

# Δημιουργούμε το συγκεκριμένο γράφημα πίττας

value_counts = df.groupby('DIR').size()

plt.figure(figsize=(12, 12))
plt.pie(value_counts, labels=value_counts.index, autopct='%1.1f%%')
plt.title('Κατανομή των ημερών σε διεύθυνση ανέμου που φυσούσε')
plt.show()

# 5. Να βρεθεί η ώρα που έχουν συμβεί οι περισσότερες μέγιστες θερμοκρασίες 
# και η ώρα με τις περισσότερες ελάχιστες θερμοκρασίες στο έτος.

# Λύση :

# # Υπολογισμός των δύο μεγεθών :
    
ωρα1 = df.groupby('TIME').size().idxmax()
ωρα2 = df.groupby('TIME.1').size().idxmax()

print(f'Η ώρα με τις περισσότερες ψηλές θερμοκρασίες ειναι {ωρα1}')
print(f'Η ώρα με τις περισσότερες χαμηλές θερμοκρασίες ειναι {ωρα2}')

# (Αντι για το df.groupby('TIME').size() θα μπορούσαμε να χρησιμοποιήσουμε και το 
# df['TIME'].value_counts() όπως και στο ερώτημα 4)

# 6. Να βρεθεί η μέρα του έτους που είχε την μεγαλύτερη διακύμανση σε θερμοκρασία.

# Λύση :
    
# Δημιουργούμε ένα καινούργιο data frame με τις στήλες από το df: MONTH, DAY και ΔΙΑΚΥΜΑΝΣΗ
# όπου τo ΔΙΑΚΥΜΑΝΣΗ είναι η διαφορά μεταξύ μεγιστης και ελάχιστης θερμοκρασίας. Aφαιρούμε
# την τελευταία στήλη με την επισκόπηση

df2 = pd.DataFrame({
    'ΜΗΝΑΣ': df.iloc[:-1, 0],
    'ΜΕΡΑ': df.iloc[:-1, 1],
    'ΔΙΑΚΥΜΑΝΣΗ': df.iloc[:-1, 3] - df.iloc[:-1, 5]
})

# Δημιουργούμε ένα τρίτο data frame το οποίο θα περιέχει μια γραμμη με τις τιμές 
# των στηλών για την μέγιστη διακύμανση θερμοκρασίας

df3 = df2[df2['ΔΙΑΚΥΜΑΝΣΗ'] == df2['ΔΙΑΚΥΜΑΝΣΗ'].max()]
print('Παρακάτω η μέρα με την μεγαλύτερη διακύμανση θερμοκρασίας:')
print(df3)

# 7. Από ποια διεύθυνση φυσούσε τις περισσότερες ημέρες του χρόνου?

# Λύση :
    
# Βρίσκουμε την τιμή της στήλης DIR που επαναλαμβανεται τις περισότερρες φορές

df['DIR'].describe()

# ή

df.groupby('DIR').size().idxmax()

# ή

df['DIR'].value_counts().idxmax()

# 8. Να βρεθεί η διεύθυνση του ανέμου που έδωσε την μεγαλύτερη ένταση ανέμου.

# Λύση :
    
# Δημιουργούμε ένα αντικείμενο a με τις μέγιστες ταχύτητες ανέμου, ομαδοποιημένες ανα κατέυθυνση
# ανεμου

a = df['W_SPEED'].groupby(df['DIR']).max()

# Aπο το συγκεκριμένο αντικείμενο, εμαφανίζουμε το index για την μεγαλύτερη αξία

print(a.idxmax())

# 9. Ποια ήταν η μέση θερμοκρασία για κάθε διεύθυνση του ανέμου? Χρησιμοποιώντας 
# τον παραγόμενο πίνακα και κατάλληλες εντολές επί αυτού να βρείτε τις διευθύνσεις 
# των ανέμων που έδωσαν την μεγαλύτερη και μικρότερη μέση θερμοκρασία.

# Λύση :

# Δημιουργούμε εναν πινακα με τις μεσες θερμοκρασίες ανα διευθυνση ανέμου
    
b = df['TEMP'].groupby(df['DIR']).mean()
print(b)

# Aπο το συγκεκριμένο αντικείμενο, εμαφανίζουμε το index για την μεγαλύτερη και την μικρότερη
# αξία

print(b.idxmax())
print(b.idxmin())

# 10. Να γίνει σε μορφή ραβδογράμματος η κατανομή του ποσού βροχόπτωσης ανά μήνα.

# Λύση :
    
# Δημιουργούμε ένα πίνακα με τις μέσες θερμοκρασίες ανα μήνα

c = df['RAIN'].groupby(df['MONTH']).mean()

# Δημιουργούμε μια λίστα με τους μήνες στον χρόνο από τον πρώτο προς τον τελευταίο

new_order = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JOU', 'JUL', 'AUG', 'SEP', 'OCT', 
             'NOV', 'DEC']

# Aνακατατάσουμε τις σειρές του πίνακα c με βάση την παραπάνω σειρά

c = c.reindex(new_order)

# Δημιουργούμε το ραβδόγραμμα

c.plot(kind='bar',x='MONTH',y='RAIN')

# 11. Να γίνει η γραμμική παλινδρόμηση για τις μέσες θερμοκρασίες του Δεκεμβρίου 
# του 2017 και να προβλέψετε μέσα από αυτή την θερμοκρασία στις 25/12 2018.

# Λύση :
    
# Δημιουργούμε εναν πίνακα Χ με μία γραμμη που θα περιέχει τις μέρες του Δεκεμβρίου.

X = df.loc[df['MONTH']== 'DEC', ['DAY']]

# Δημιουργούμε εναν πίνακα Y με μία γραμμη που θα περιέχει τις μέσες θερμοκρασίες
# του Δεκεμβρίου ανά ημέρα.

Y = df.loc[df['MONTH']== 'DEC', ['TEMP']]

# Δημιουργούμε εναν πίνακα I με 31 μοναδιαίες τιμές και τέλος εναν πίνακα Α με πρωτη
# στήλη τις τιμές του Χ και δεύτερη τις τιμές του Ι

I = np.ones(31,)
A=np.c_[X, I]

# Δημιουργούμε τον τύπο για την ευθεία ελαχίστων τετραγώνων

np.linalg.lstsq(A,Y,rcond='warn')

# Με τις τιμές που παίρνουμε για τα α και β κάνουμε πρόβλεψη για το Υ όταν το Χ είναι 25

xp=25
yp = -0.18048387*xp+ 14.08129032
print(yp)

#To αποτέλεσμα ειναι 9.569193570000001

# 12. Να γίνουν 4 γραφικές υπο-παραστάσεις που κάθε μια θα αφορά μια εποχή: 
#     Χειμώνας(Δεκ17-Φεβ17)-Άνοιξη (Μαρ-Μαϊ), Καλοκαίρι(Ιουν-Αυγ), Φθινόπωρο (Σεπ-Νοε). 
#     Σε κάθε μια θα φαίνεται η μέση θερμοκρασία με πράσινο χρώμα, η μέγιστη με κόκκινο 
#     και η ελάχιστη με μπλε.

# Λύση :
    
# Δημιουργούμε εναν υποπίνακα απο το df στο οποίο ο Μηνας να είναι Δεκέμβριος, Ιανουάριος 
# η Φρεβρουάριος

filter1 = ['DEC', 'JAN','FEB']
condition1 = df['MONTH'].isin(filter1)
fdf1 = df[condition1]

# Kάνουμε το ίδιο και για τις υπόλοιπες εποχές

filter2 = ['MAR', 'APR','MAY']
condition2 = df['MONTH'].isin(filter2)
fdf2 = df[condition2]

filter3 = ['JOU', 'JUL','AUG']
condition3 = df['MONTH'].isin(filter3)
fdf3 = df[condition3]

filter4 = ['SEP', 'OCT','NOV']
condition4 = df['MONTH'].isin(filter4)
fdf4 = df[condition4]

# Για να απεικονίσουμε τις θερμοκρασίες ανα εποχή, θα φτιάξουμε 4 διαγράμματα στηλών. 
# Δημιουργούμε τις κατηγορίες, χρώμα για κάθε κατηγορία και τις τιμές για κάθε εποχή

Κατηγορίες = [ 'Μέγιστη', 'Μέση', 'Χαμηλή']
colors = ['red', 'green', 'blue']
Τιμές1 = [fdf1['HIGH'].mean(), fdf1['TEMP'].mean(),fdf1['LOW'].mean()]
Τιμές2 = [fdf2['HIGH'].mean(), fdf2['TEMP'].mean(),fdf2['LOW'].mean()]
Τιμές3 = [fdf3['HIGH'].mean(), fdf3['TEMP'].mean(),fdf3['LOW'].mean()]
Τιμές4 = [fdf4['HIGH'].mean(), fdf4['TEMP'].mean(),fdf4['LOW'].mean()]

#  Δημιουργούμε το γράφημα

plt.subplot(221)

plt.bar(Κατηγορίες, Τιμές1, color= colors)
plt.ylabel('Θερμοκρασίες')
plt.title('Χειμώνας')

plt.subplot(222)

plt.bar(Κατηγορίες, Τιμές2, color= colors)
plt.ylabel('Θερμοκρασίες')
plt.title('Ανοιξη')

plt.subplot(223)

plt.bar(Κατηγορίες, Τιμές3, color= colors)
plt.ylabel('Θερμοκρασίες')
plt.title('Καλοκαίρι')

plt.subplot(224)

plt.bar(Κατηγορίες, Τιμές4, color= colors)
plt.ylabel('Θερμοκρασίες')
plt.title('Φθινόπωρο')

plt.tight_layout()
plt.show()

# 13. Να φτιαχτεί συνάρτηση που θα παίρνει ως όρισμα το άθροισμα των ποσών βροχόπτωσης 
# και αν είναι < 400 θα επιστρέφει «Λειψυδρία», >=400 & <600 «Ικανοποιητικά ποσά βροχής» 
# ενώ πάνω από 600 θα γράφει «Υπερβολική βροχόπτωση»

# Λύση :

def rain_condition(RAIN):
    if RAIN < 400:
        return "Λειψυδρία"
    elif 400 <= RAIN < 600:
        return "Ικανοποιητικά ποσά βροχής"
    elif RAIN >= 600:
        return "Υπερβολική βροχόπτωση"
    
RAIN = df['RAIN'].sum()

rain_condition(RAIN)
 
