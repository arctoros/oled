from PIL import Image, ImageOps
import sys

original = Image.open(sys.argv[1])
file = open('image.bin', 'wb')
image = original.resize((128,64)).convert("1")
image.show()
pixels = image.load()

for x in range(128):
    for y in range(8):
        result = 0
        for z in range(8):
            pixel = int(pixels[x, y * z] / 255)
            result += pixel * pow(2,z)
        result = result.to_bytes(1, byteorder='little')
        file.write(result)
