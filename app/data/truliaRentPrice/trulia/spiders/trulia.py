import scrapy
import re
class QuotesSpider(scrapy.Spider): 
	name = "trulia" #identifies the spider. It must be a unique within a project.
	
		#must return an iterable of Requests
	start_urls=['https://www.trulia.com/for_rent/New_York,NY/']
		
		
			
			
	def parse(self, response):   #parses the response, extract data and save it as a dic. Also, find new URL to follow and create requests.
		
		cards=response.xpath("//div[@class='card backgroundBasic']")
		for href in cards.xpath("//ul//li//a/@href").extract(): #get the link for each movie
			
			yield scrapy.Request(response.urljoin(href),callback=self.parse_property)
			
			
		
		next_page=response.xpath("//div[@id='resultsColumn']/div/div[2]/div[2]/div[1]/div[1]/span[4]/a/@href").extract_first()
		if next_page is not None:
			yield scrapy.Request(next_page, callback=self.parse)    
		
		
		
	
	def parse_property(self,response):
		addr=response.xpath("//div[@id='propertyDetails']/div/div[2]/span/span[1]/text()").extract_first()
		
		
		
		
		bedrooms=response.xpath("//div[@id='propertyDetails']/div/ul[1]/li[1]/text()").extract_first()
		bathrooms=response.xpath("//div[@id='propertyDetails']/div/ul[1]/li[2]/text()").extract_first()
		price=response.xpath("//div[@id='rentalPdpContactLeadForm']/div[1]/div/span/text()").extract_first()
		
		
		
		if addr==None:
			addr=response.xpath("//div[@id='propertyDetails']/div/div[2]/h1/span/text()").extract_first()
		
		# extract specific bedrooms:
		
			
		if len(re.findall(". - ([0-9])",bedrooms))!=0:
			bedrooms=re.findall(". - ([0-9])",bedrooms)[0]
		else:
			bedrooms=re.findall("[0-9]",bedrooms)[0]
		
		
		# extract specific bathrooms:
		
		if len(re.findall(". - ([0-9])",bathrooms))!=0:
			bathrooms=re.findall(". - ([0-9])",bathrooms)[0]
		else:
			bathrooms=re.findall("[0-9]",bathrooms)[0]
		
	
		# extract specific price:
		
		
		if len(re.findall(".*\$(.*?)/mo",price))!=0:
			price=re.findall(".*\$(.*?)/mo",price)[0]
			
		
	
		yield{
		'addr':addr,
		'bedrooms':bedrooms,
		'bathrooms':bathrooms,
		'price':price
		}
		