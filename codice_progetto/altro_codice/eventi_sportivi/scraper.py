from selenium import webdriver
from selenium.webdriver.chrome.options import Options 
from bs4 import BeautifulSoup
import time
import pandas as pd

df = pd.DataFrame(columns = ['Squadra1', 'Squadra2', 'Date', 'Hours', 'Diretta'])

squadre=["Inter", "Milan", "Juventus", "Bologna", "Spal", "Parma", "Sassuolo"]
listpartita=[]

DRIVER_PATH = 'C://Users/Stefano/Downloads/chromedriver.exe'
driver = webdriver.Chrome(executable_path=DRIVER_PATH)
counter=1
while counter <= 38:
    driver.get('https://www.legaseriea.it/it/serie-a/calendario-e-risultati/2020-21/UNICO/UNI/'+str(counter))
    time.sleep(5) #aspetto che la pagina si carica se no rompe
    SCROLL_PAUSE_TIME = 0.1 #tempo tra uno scroll e l'altro
    #limito l'area dove scrollo per metterci meno
    i = 0 
    f = driver.execute_script("return document.body.scrollHeight")-2000
    while True:
        j=i+100
        driver.execute_script("window.scrollTo("+str(i)+","+str(j)+" );")
        i+=100
        time.sleep(SCROLL_PAUSE_TIME)
        new_height = driver.execute_script("return document.body.scrollHeight")
        if i >= f:
            break
    soup = BeautifulSoup(driver.page_source,features="html.parser")
    partite_a = soup.find_all("div", {"class": "box-partita col-xs-12 col-sm-4 col-md-3"})
    partite_b = soup.find_all("div", {"class": "box-partita dark col-xs-12 col-sm-4 col-md-3"}) 
    for partita in partite_a:
        squadra1=partita.find("div", {"class": "col-xs-6 risultatosx"})
        squadra2=partita.find("div", {"class": "col-xs-6 risultatodx"})
        squadra1=squadra1.find("h4", {"class": "nomesquadra"}).getText()
        squadra2=squadra2.find("h4", {"class": "nomesquadra"}).getText()
        if any(squadra1 in s for s in squadre) | any(squadra2 in s for s in squadre) :
            listpartita.append(squadra1)
            listpartita.append(squadra2)
            dati = partita.find("div", {"class": "datipartita"})
            p=dati.find('p')
            data_ora = p.find('span').getText()
            posizione= data_ora.index(" ")
            data = data_ora[0:posizione]
            ora=data_ora[posizione+1:]
            listpartita.append(data)
            listpartita.append(ora)
            p=p.getText()
            posizione= p.index("Diretta")
            p=p[posizione:]
            p = p.replace('\n','')
            p= p.replace('\t','')
            listpartita.append(p.strip())
            df.loc[len(df.index)] = listpartita
            listpartita=[]
            print(p)
    for partita in partite_b:
        squadra1=partita.find("div", {"class": "col-xs-6 risultatosx"})
        squadra2=partita.find("div", {"class": "col-xs-6 risultatodx"})
        squadra1=squadra1.find("h4", {"class": "nomesquadra"}).getText()
        squadra2=squadra2.find("h4", {"class": "nomesquadra"}).getText()
        if any(squadra1 in s for s in squadre) | any(squadra2 in s for s in squadre) :
            listpartita.append(squadra1)
            listpartita.append(squadra2)
            dati = partita.find("div", {"class": "datipartita"})
            p=dati.find('p')
            data_ora = p.find('span').getText()
            posizione= data_ora.index(" ")
            data = data_ora[0:posizione]
            ora=data_ora[posizione+1:]
            listpartita.append(data)
            listpartita.append(ora)
            p=p.getText()
            posizione= p.index("Diretta")
            p=p[posizione:]
            p = p.replace('\n','')
            p= p.replace('\t','')
            listpartita.append(p.strip().strip('"'))
            df.loc[len(df.index)] = listpartita
            listpartita=[]
    counter+=1
df.to_csv('C://Users/Stefano/Downloads/2020-21.csv', index=False)


            


