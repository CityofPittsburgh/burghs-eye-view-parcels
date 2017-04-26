import cx_Oracle
import unicodecsv as csv
from datetime import date

today = date.today() 

first = date(today.year, 01, 01)

datefirst = first.strftime("%m/%d/%Y")
datesecond = today.strftime("%m/%d/%Y")


connection = cx_Oracle.connect('REALESTATE/prod@hercules.city.pittsburgh.pa.us:1521/RESTATE')
cursor = connection.cursor()

query ="select distinct master.acct_no CITY_PIN, master.cnty_owner_name OWNER, MASTER.PROP_LOW_HOUSE_NO || ' ' || master.prop_street_name ADDRESS, \
master.prop_zip, master.last_sale_date, master.bankrupt_flag, \
master.abatement_flag, master.neighborhood, master.homestead, web_delinquents.current_delq_tax, city_county_accounts.cnty_acct COUNTY_PIN, \
abatement_programs.program_name, abatement_programs.no_years ABATEMENT_LENGTH, account_abatements.start_year, account_abatements.approved_user \
FROM master \
FULL OUTER JOIN web_delinquents \
ON MASTER.ACCT_NO = web_delinquents.acct_no \
INNER JOIN city_county_accounts \
ON master.acct_no = city_county_accounts.city_acct \
FULL OUTER JOIN account_abatements \
ON master.master_seq = account_abatements.master_seq \
FULL OUTER JOIN abatement_programs \
ON account_abatements.abatement_key = abatement_programs.rec_no"
               
cursor.execute(query)

with open('test_city_parcels.csv', 'w') as file:
    writer = csv.writer(file,  delimiter=',', lineterminator='\n', quotechar='"')
    writer.writerow([i[0] for i in cursor.description])
    writer.writerows(cursor)
    for row in cursor:
        writer.writerow(row)
    connection.close()
