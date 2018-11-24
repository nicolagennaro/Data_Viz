import re



with open("laws.html") as f:
    content = f.readlines()


data = {'code': [],
        'State': [],
        'Legal Status': [],
        'Medicinal': [],
        'Decriminalized': []
}



for i, line in enumerate(content):


    id = re.search('id="(..)"', line)

    if( id is not None):
        data['code'].append(id.group(1))

        state = re.search('<div><h1>(.*)</h1></div>', line)
        data['State'].append(state.group(1))

        ls = re.search('<div><p>Legal Status:\s*(.{0,16})</p></div>', line)
        data['Legal Status'].append(ls.group(1))


        med = re.search('<div><p>Medicinal:\s*(.{0,10})</p></div>', line)
        data['Medicinal'].append(med.group(1))


        dec = re.search('<div><p>Decriminalized:\s*(.{0,10})</p></div>', line)
        data['Decriminalized'].append(dec.group(1))



import pandas as pd


pd.DataFrame(data).to_csv(path_or_buf='clean_laws.csv', header=True, index=False)


