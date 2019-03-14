
# En Haskell:
# suma x y = x + y

# En Python:

def suma(x):
  def sumax(y):
    return x + y
  return sumax

print suma(2)
print suma(2)(3)
