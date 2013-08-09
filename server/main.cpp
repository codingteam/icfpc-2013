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

#include "expr.h"

boost::property_tree::ptree HTTPGet(const std::string &path)
{
	boost::property_tree::ptree response;
	try
	{
		curlpp::Cleanup cleaner;
		curlpp::Easy http_request;
		http_request.setOpt<curlpp::options::Url>("http://icfpc2013.cloudapp.net/" + path + "?auth=0379MPEZKNzwqnYUu1DMm7zn2uyo6oflLxR0vukWvpsH1H");
		http_request.setOpt<curlpp::options::Verbose>(true);
		std::stringstream resp_stream;
		http_request.setOpt<curlpp::options::WriteStream>(&resp_stream);
		http_request.perform();
		boost::property_tree::json_parser::read_json(resp_stream, response);
	}
	catch(curlpp::LogicError &e)
	{
		std::cout << e.what() << std::endl;
		throw;
	}
	catch(curlpp::RuntimeError &e)
	{
		std::cout << e.what() << std::endl;
		throw;
	}
	catch(...)
	{
		std::cerr << "Exception." << std::endl;
		throw;
	}
	return response;
}

boost::property_tree::ptree HTTPPost(const std::string &path, const boost::property_tree::ptree& request)
{
	std::stringstream req_stream;
	boost::property_tree::json_parser::write_json(req_stream, request);
	boost::property_tree::ptree response;
	try
	{
		curlpp::Cleanup cleaner;
		curlpp::Easy http_request;
		http_request.setOpt<curlpp::options::Url>("http://icfpc2013.cloudapp.net/" + path + "?auth=0379MPEZKNzwqnYUu1DMm7zn2uyo6oflLxR0vukWvpsH1H");
		http_request.setOpt<curlpp::options::Verbose>(true);
		std::list<std::string> header; 
		header.push_back("Content-Type: application/json"); 
    		http_request.setOpt<curlpp::options::HttpHeader>(header); 
		http_request.setOpt<curlpp::options::PostFields>(req_stream.str());
		http_request.setOpt<curlpp::options::PostFieldSize>(req_stream.str().size());
		std::stringstream resp_stream;
		http_request.setOpt<curlpp::options::WriteStream>(&resp_stream);
		http_request.perform();
		boost::property_tree::json_parser::read_json(resp_stream, response);
	}
	catch(curlpp::LogicError &e)
	{
		std::cout << e.what() << std::endl;
		throw;
	}
	catch(curlpp::RuntimeError &e)
	{
		std::cout << e.what() << std::endl;
		throw;
	}
	catch(...)
	{
		std::cerr << "Exception." << std::endl;
		throw;
	}
	return response;
}


void Guess(const std::string& id, const std::string& program)
{	
	boost::property_tree::ptree request;
	request.put("id", id);
	request.put("program", program);

	auto response = HTTPPost("guess", request);
	boost::property_tree::json_parser::write_json(std::cout, response);
}

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
	else
	{
		request.put("operators", "");
	}

	auto response = HTTPPost("train", request);

	std::cout << "Id: " << response.get<std::string>("id") << std::endl;
	std::cout << "Size: " << response.get<int>("size") << std::endl;
	for(const auto& item : response.get_child("operators"))
	{
		std::cout << " Operator=" << item.second.get<std::string>("") << std::endl;
	}
	std::cout << "Challenge: " << response.get<std::string>("challenge") << std::endl;

	Guess(response.get<std::string>("id"), response.get<std::string>("challenge"));

	return 0;
}

