import pandas as pd
from update_stnmas import read_stnmas, write_stnmas

inputf = 'test_data/abrv_stnlist'
outf = 'test_data/new_stnmas'

df = read_stnmas(inputf)
print(df.head())
print(df.info())
long = df.iloc[1]["LONG"]
print(f'{long} length {len(long)}')
write_stnmas(df, outf)
