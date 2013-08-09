#include <iostream>
#include <cstring>
#include <cstdlib>
#include <sstream>

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>
#include <curlpp/Exception.hpp>

int main(int argc, char** argv)
{
	int req_size = 0;
	const char *req_operators = "";

	for(int i = 1; i < argc; ++ i)
	{
		if(strcmp(argv[i], "fold") == 0)
		{
			req_operators = "fold";
		}
		else if(strcmp(argv[i], "tfold") == 0)
		{
			req_operators = "tfold";
		}
		else 
		{
			req_size = atoi(argv[i]);
			if((req_size > 30) || (req_size < 3))
			{
				std::cerr << "Usage: " << argv[0] << " [tfold | fold] [3-30]" << std::endl;
				return 1;
			}
		}
	}

	// send request
	boost::property_tree::ptree request;
	if(req_size > 0)
	{
		request.put("size", req_size);
	}
	if((req_operators != nullptr) && (req_operators[0] != '\0'))
	{
		request.put("operators", req_operators);
	}

	std::stringstream req_stream;
	boost::property_tree::json_parser::write_json(req_stream, request);

	try
	{
		curlpp::Cleanup cleaner;
		curlpp::Easy request;
		request.setOpt<curlpp::options::Url>("http://icfpc2013.cloudapp.net/train?auth=0379MPEZKNzwqnYUu1DMm7zn2uyo6oflLxR0vukWvpsH1H");
		request.setOpt<curlpp::options::Verbose>(true);

		std::list<std::string> header; 
		header.push_back("Content-Type: application/json"); 
    
		request.setOpt<curlpp::options::HttpHeader>(header); 

		request.setOpt<curlpp::options::PostFields>(req_stream.str());
		request.setOpt<curlpp::options::PostFieldSize>(req_stream.str().size());
		request.perform();
	}
	catch(curlpp::LogicError &e)
	{
		std::cout << e.what() << std::endl;
	}
	catch(curlpp::RuntimeError &e)
	{
		std::cout << e.what() << std::endl;
	}
	catch(...)
	{
		std::cerr << "Exception." << std::endl;
	}

	

	return 0;
}

