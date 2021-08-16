import pandas as pd
import datetime
import numpy as np
import math
import openpyxl
from tkinter import filedialog
import tkinter as tk

win = tk.Tk()
win.withdraw()
sourcefile = filedialog.askopenfilename(initialdir='C:/Users/Antoni.Wojcik/PycharmProjects/Średnie_sprzedaży_action',
                                        title='Otwieranie')

print(f'Wybrana ścieżka: {sourcefile}')
win.destroy()

df = pd.read_excel(sourcefile)
df.drop([df.index[0]], inplace=True)  # deleting 1st row

# Extracting names and storing it in numpy array
name_column = df.iloc[:, 0]
names = name_column.values

# Dropping columns from 1-11 and 120-128
df.drop(df.columns[range(11)], axis=1, inplace=True)
df.drop(df.columns[range(120, 127)], axis=1, inplace=True)


base = datetime.datetime.today()
date_list = [base - datetime.timedelta(days=x) for x in range(61)]
del date_list[0]
date_list.reverse()

weekend = []
dates = []
for idx, val in enumerate(date_list):
    dates.append(str(val)[0:10])
    dates.append(str(val)[0:10])
    case = val.isoweekday()
    if case == 6 or case == 7:
        weekend.append(str(val)[0:10])


df.set_axis(dates, axis='columns', inplace=True)  # deleting column names
df.drop(weekend, axis=1, inplace=True)  # deleting weekends


column_quantity = len(df.columns)
to_delete_quantity = column_quantity - 64
df.drop(columns=df.columns[:to_delete_quantity], axis=1, inplace=True)  # deleting columns until 64 are reached

rows_count = len(df.index)

names_counter = 0
matrix_names_and_values = []

for i in range(rows_count):
    current_sale_counter = 0  # Counter for selecting pairs sale<->facture
    extracted_row = df.iloc[[i]].values[0].tolist()
    sales = extracted_row[0::2]
    factures = extracted_row[1::2]

    sales_end = []
    factures_end = []
    sales_end_2 = []
    factures_end_2 = []
    sales_end_final = []

    for index, value in enumerate(sales):
        if math.isnan(sales[index]) is True and math.isnan(factures[index]) is True:
            pass
        elif math.isnan(sales[index]) is True and math.isnan(factures[index]) is False:
            sales_end.append(sales[index])
            factures_end.append(np.nan)
        elif math.isnan(sales[index]) is False and math.isnan(factures[index]) is True:
            sales_end.append(sales[index])
            factures_end.append(0)
        elif math.isnan(sales[index]) is False and math.isnan(factures[index]) is False:
            sales_end.append(sales[index])
            factures_end.append(factures[index])

        sales_end_2 = [x for x in sales_end if ~np.isnan(x)]
        factures_end_2 = [x for x in factures_end if ~np.isnan(x)]

    if not sales_end_2:  # If the list is empty -> []
        matrix_names_and_values.extend([names[names_counter], '0'])
        names_counter = names_counter+1
    else:
        np.array(sales_end_2)
        quantile_25 = np.quantile(sales_end_2, .25)
        quantile_75 = np.quantile(sales_end_2, .75)

        IQR = (quantile_75 - quantile_25) * 0.2412
        Q1 = quantile_25 - IQR
        Q3 = quantile_75 + IQR

        if quantile_75 == 0 and quantile_25 == 0:
            mean = np.mean(sales_end_2)
            standard_deviation = np.std(sales_end_2, ddof=1)
            top_border = mean + standard_deviation
            bottom_border = mean - standard_deviation

            for sale in sales_end_2:
                current_sale = sales_end_2[current_sale_counter]
                current_facture = factures_end_2[current_sale_counter]
                if sale >= bottom_border and sale <= top_border:
                    sales_end_final.append(current_sale)
                elif current_sale == 0:
                    pass
                elif (current_facture/current_sale) * 100 > 20:
                    sales_end_final.append(current_sale)
                current_sale_counter = current_sale_counter + 1
            if len(sales_end_2) < 5:
                matrix_names_and_values.extend([names[names_counter], '0'])
            else:
                if sum(sales_end_final) / len(sales_end_final) < 0:
                    matrix_names_and_values.extend([names[names_counter], '0'])
                else:
                    matrix_names_and_values.extend([names[names_counter], sum(sales_end_final) / len(sales_end_final)])
            names_counter = names_counter + 1
        else:
            for sale in sales_end_2:
                current_sale = sales_end_2[current_sale_counter]
                current_facture = factures_end_2[current_sale_counter]
                if sale >= Q1 and sale <= Q3:
                    sales_end_final.append(current_sale)
                elif current_sale == 0:
                    pass
                elif (current_facture/current_sale) * 100 > 20:
                    sales_end_final.append(current_sale)
                current_sale_counter = current_sale_counter + 1
            if len(sales_end_2) < 5:
                matrix_names_and_values.extend([names[names_counter], '0'])
            else:
                if sum(sales_end_final) / len(sales_end_final) < 0:
                    matrix_names_and_values.extend([names[names_counter], '0'])
                else:
                    matrix_names_and_values.extend([names[names_counter], sum(sales_end_final) / len(sales_end_final)])
            names_counter = names_counter + 1

matrix_names_and_values_numpy = np.reshape(matrix_names_and_values, (int(len(matrix_names_and_values)/2), 2))
df_end = pd.DataFrame(matrix_names_and_values_numpy)
df_end.to_excel('out.xlsx', index=False)
