from bs4 import BeautifulSoup
import requests
import re

session = requests.Session()
session.headers.update({
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.9',
    'Accept-Encoding': 'gzip, deflate, br',
    'DNT': '1',
    'Connection': 'keep-alive',
    'Upgrade-Insecure-Requests': '1',
    'Sec-Fetch-Dest': 'document',
    'Sec-Fetch-Mode': 'navigate',
    'Sec-Fetch-Site': 'none',
    'Sec-Fetch-User': '?1',
    'Cache-Control': 'max-age=0',
})

# Test with George A. Flores page
url = "https://nycdetectives.org/honor-roll/george-a-flores/"
page = session.get(url, timeout=10)
soup = BeautifulSoup(page.text, 'html.parser')

print("=== PAGE TITLE ===")
print(soup.title.text if soup.title else "No title")

print("\n=== LOOKING FOR SHIELD NUMBER ===")
shield_elements = soup.find_all(text=re.compile(r'Shield Number', re.IGNORECASE))
for i, elem in enumerate(shield_elements):
    print(f"Found {i+1}: {repr(elem)}")
    parent = elem.parent
    if parent:
        print(f"  Parent tag: {parent.name}")
        print(f"  Parent class: {parent.get('class', [])}")
        print(f"  Parent text: {parent.get_text(strip=True)}")

print("\n=== LOOKING FOR COMMAND ===")
command_elements = soup.find_all(text=re.compile(r'Command', re.IGNORECASE))
for i, elem in enumerate(command_elements):
    print(f"Found {i+1}: {repr(elem)}")
    parent = elem.parent
    if parent:
        print(f"  Parent text: {parent.get_text(strip=True)}")

print("\n=== LOOKING FOR BIOGRAPHY ===")
bio_content = soup.find('div', class_='elementor-theme-post-content')
if bio_content:
    print("Found biography content:")
    print(bio_content.get_text(strip=True)[:500])
else:
    print("No biography content found")

print("\n=== ALL H6 ELEMENTS ===")
h6_elements = soup.find_all('h6')
for i, h6 in enumerate(h6_elements[:20]):  # Show first 20
    text = h6.get_text(strip=True)
    if text and len(text) < 100:  # Filter out very long/empty ones
        print(f"{i+1:2d}. {text}")
