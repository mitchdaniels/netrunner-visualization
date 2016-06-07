import requests
import csv
from datetime import date, timedelta
from dateutil.parser import parse

# Get JSON of all cards from NetrunnerDB
def getCards():
	cards = requests.get('http://netrunnerdb.com/api/cards').json()
	return cards

# Get JSON of all decks from NetrunnerDB
def getDecks(start, end):
	decks = []

	startDate 	= parse(start)
	endDate 	= parse(end)
	delta 		= endDate - startDate

	for i in range(delta.days + 1):

		date = startDate + timedelta(days=i)

		r = requests.get('http://netrunnerdb.com/api/decklists/by_date/' + date.strftime("%Y-%m-%d")).json()
		decks.extend(r)

		print date

	return decks

# Transform cards JSON to CSV flat-file
def createCardsCSV(cards):
	with open('cards.csv', 'wb') as csvfile:
		keys = set()

		for card in cards:
			for key in card.keys():
				keys.add(key)

		writer = csv.DictWriter(csvfile, keys)
		writer.writeheader()

		for card in cards:
		    writer.writerow({k: unicode(v).encode('utf-8') for k, v in card.iteritems()})

# Create deckContents.csv
def createDecksCSV(decks):
	with open('decks.csv', 'wb') as csvfile:
		fieldnames = ['id', 'card', 'copies']
		writer = csv.writer(csvfile)
		writer.writerow(fieldnames)

		for deck in decks:
			for card, number in deck['cards'].iteritems():
				writer.writerow([deck['id'], str(card), number])

# Transform decks JSON to CSV flat-file
def createDeckMetadataCSV(decks):
	with open('deckMetadata.csv', 'wb') as csvfile:
		fieldnames = ['id', 'creation', 'username']
		writer = csv.writer(csvfile)
		writer.writerow(fieldnames)

		for deck in decks:
			writer.writerow([deck['id'], deck['date_creation'], deck['username'].encode('utf-8')])