from bs4 import BeautifulSoup
import requests
import csv
import time
import re

BASE_URL = "https://nycdetectives.org/honor-roll/"

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

def extract_detail_info(detective_url):
    """Extract detailed information from a detective's individual page"""
    try:
        time.sleep(0.5)  # Be respectful to the server
        page = session.get(detective_url, timeout=10)
        soup = BeautifulSoup(page.text, 'html.parser')
        
        detail_info = {}
        
        # Find all h6 elements and extract structured data
        h6_elements = soup.find_all('h6', class_='elementor-heading-title')
        
        for i, h6 in enumerate(h6_elements):
            text = h6.get_text(strip=True)
            
            # Look for label elements and get the next element as value
            if text in ['Shield Number:', 'Command:', 'Date of Death:', 'Cause of Death:', 'Rank:']:
                if i + 1 < len(h6_elements):
                    value = h6_elements[i + 1].get_text(strip=True)
                    
                    if text == 'Shield Number:':
                        detail_info['shield_number'] = value
                    elif text == 'Command:':
                        detail_info['command'] = value
                    elif text == 'Date of Death:':
                        detail_info['date_of_death_detail'] = value
                    elif text == 'Cause of Death:':
                        detail_info['cause_of_death'] = value
                    elif text == 'Rank:':
                        detail_info['rank_detail'] = value
        
        # Extract biography from theme post content
        bio_content = soup.find('div', class_='elementor-theme-post-content')
        if bio_content:
            bio_text = bio_content.get_text(strip=True)
            if bio_text:
                detail_info['biography'] = bio_text
        
        return detail_info
        
    except Exception as e:
        print(f"Error fetching details for {detective_url}: {e}")
        return {}

def scrape_detectives_with_details():
    try:
        page = session.get(BASE_URL, timeout=10)
        soup = BeautifulSoup(page.text, 'html.parser')
        
        # Find the honor roll table
        table = soup.find('table', {'id': 'myTable'})
        if not table:
            print("Table not found!")
            return []
        
        detectives = []
        rows = table.find('tbody').find_all('tr')
        
        print(f"Found {len(rows)} detective entries in the table")
        print("Extracting detailed information for each detective...")
        
        for i, row in enumerate(rows, 1):
            cells = row.find_all('td')
            if len(cells) >= 3:
                # Extract name from the link
                name_cell = cells[0]
                name_link = name_cell.find('a')
                name = name_link.get_text(strip=True) if name_link else name_cell.get_text(strip=True)
                
                # Extract basic info from table
                rank = cells[1].get_text(strip=True)
                date_of_death = cells[2].get_text(strip=True)
                
                detective_info = {
                    'name': name,
                    'rank': rank,
                    'date_of_death': date_of_death
                }
                
                # Get the URL for detailed information
                if name_link and name_link.get('href'):
                    detail_url = name_link['href']
                    print(f"({i}/{len(rows)}) Fetching details for: {name}")
                    detail_info = extract_detail_info(detail_url)
                    detective_info.update(detail_info)
                
                detectives.append(detective_info)
        
        return detectives
        
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        return []

def main():
    print("Scraping NYC Detectives Honor Roll with detailed information...")
    detectives = scrape_detectives_with_details()
    
    if detectives:
        print(f"\nSuccessfully scraped {len(detectives)} detectives with detailed information")
        
        # Show first few entries as example
        print("\nFirst 3 entries with details:")
        for i, detective in enumerate(detectives[:3], 1):
            print(f"\n{i}. {detective.get('name', 'N/A')}")
            print(f"   Rank: {detective.get('rank', 'N/A')}")
            print(f"   Date of Death: {detective.get('date_of_death', 'N/A')}")
            if 'shield_number' in detective:
                print(f"   Shield Number: {detective['shield_number']}")
            if 'command' in detective:
                print(f"   Command: {detective['command']}")
            if 'cause_of_death' in detective:
                print(f"   Cause of Death: {detective['cause_of_death']}")
            if 'biography' in detective:
                bio = detective['biography'][:200] + "..." if len(detective['biography']) > 200 else detective['biography']
                print(f"   Biography: {bio}")
        
        # Save to CSV with all fields
        fieldnames = ['name', 'rank', 'date_of_death', 'shield_number', 'command', 'cause_of_death', 'rank_detail', 'date_of_death_detail', 'biography']
        
        with open('detectives_detailed.csv', 'w', newline='', encoding='utf-8') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            for detective in detectives:
                writer.writerow(detective)
        
        print(f"\nSaved {len(detectives)} detailed entries to detectives_detailed.csv")
    else:
        print("No detective data found.")

if __name__ == "__main__":
    main()
