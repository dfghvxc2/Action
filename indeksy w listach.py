import datetime
import time


def main():
    base = datetime.datetime.today()
    date_list = [base - datetime.timedelta(days=x) for x in range(1, 100000)]
    date_list.reverse()

    weekend = [date.strftime("%m/%d/%Y") for date in date_list if (date.isoweekday() == 6 or date.isoweekday() == 7)]
    dates = []
    for date in date_list:
        dates.extend([date.strftime("%m/%d/%Y"), date.strftime("%m/%d/%Y")])


def main2():
    base = datetime.datetime.today()
    date_list = [base - datetime.timedelta(days=x) for x in range(100000)]
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


start_time = time.time()
main()
print("--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
main2()
print("--- %s seconds ---" % (time.time() - start_time))




