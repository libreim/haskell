
# suma x y = x + y es equivalente a:
def suma(x):
  def sumax(y):
    return x + y
  return sumax

print suma(2)(3)
