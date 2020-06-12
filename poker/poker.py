# https://projecteuler.net/problem=54

from pyswip import Prolog

def _ranknum(rank):
  if rank not in ['A', 'K', 'Q', 'J', 'T']:
     return rank
  return { 'A': '14', 'K': '13', 'Q':'12', 'J':'11', 'T':'10' }[rank]

def _query(line):
  cards = line.split(' ')
  listed_cards = [ '[' + _ranknum(c[0]) + ',' +  c[1].lower() + ']'
                  for c in cards]
  hand1 = '[' + ', '.join(listed_cards[0:5]) + ']'
  hand2 = '[' + ', '.join(listed_cards[5:10]) + ']'

  return f'winner({hand1}, {hand2})'

def _compute(line):
  prolog = Prolog()
  prolog.consult('poker.pl')

  q = prolog.query(_query(line))

  for r in q:
    return True
    break

  return False

def main():

  count = 0

  with open('poker.txt', 'r') as f:
    for line in f:

      if(_compute(line)):
        count += 1

  print(count)
  return(count)

if __name__ == '__main__':
  main()
