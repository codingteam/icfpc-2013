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
#include "generator.h"

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

void PrintProgram(std::ostream& os, const Expr& prog)
{
	Printer prn(os);
	os << "(lambda(x_0)";
	boost::apply_visitor(prn, prog);
	os << ")";
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
	boost::property_tree::ptree req_operators;

	for(int i = 1; i < argc; ++ i)
	{
		if(strcmp(argv[i], "fold") == 0)
		{
			boost::property_tree::ptree child;
			child.put("", "fold");
			req_operators.push_back(std::make_pair("", child));
		}
		else if(strcmp(argv[i], "tfold") == 0)
		{
			boost::property_tree::ptree child;
			child.put("", "tfold");
			req_operators.push_back(std::make_pair("", child));
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


#if 0
	// send request
	boost::property_tree::ptree request;
	if(req_size > 0)
	{
		request.put("size", req_size);
	}

	request.add_child("operators", req_operators);

	boost::property_tree::json_parser::write_json(std::cout, request);

	auto response = HTTPPost("train", request);

	std::cout << "Id: " << response.get<std::string>("id") << std::endl;
	std::cout << "Size: " << response.get<int>("size") << std::endl;
	for(const auto& item : response.get_child("operators"))
	{
		std::cout << " Operator=" << item.second.get<std::string>("") << std::endl;
	}
	std::cout << "Challenge: " << response.get<std::string>("challenge") << std::endl;

 #if 0
	Guess(response.get<std::string>("id"), response.get<std::string>("challenge"));
 #endif
#endif

#if 0

	std::stringstream ss;
	Expr prog = Op1<Shr1>(Id(0));
	PrintProgram(ss, prog);

	std::cout << ss.str() << std::endl;

	boost::property_tree::ptree request;
	request.put("program", ss.str());

	boost::property_tree::ptree req_args;

	for(const auto& arg : {"0x00000000000001", "0xEFFFFFFFFFFFFF"})
	{
		boost::property_tree::ptree child;
		child.put("", arg);
		req_args.push_back(std::make_pair("", child));
	}

	request.add_child("arguments", req_args);

	boost::property_tree::json_parser::write_json(std::cout, request);

	auto response = HTTPPost("eval", request);

	boost::property_tree::json_parser::write_json(std::cout, response);

#endif

#if 1
	Ops ops;
	ops.Set<Ops::NOT>();
	//ops.Set<Ops::SHL1>();
	//ops.Set<Ops::SHR1>();
	//ops.Set<Ops::SHR4>();
	//ops.Set<Ops::SHR16>();

	//ops.Set<Ops::AND>();
	ops.Set<Ops::OR>();
	//ops.Set<Ops::XOR>();
	//ops.Set<Ops::PLUS>();

	//ops.Set<Ops::IF0>();
	//ops.Set<Ops::TFOLD>();
	ops.Set<Ops::FOLD>();

	auto res = Generate(req_size, ops);
	std::cout << "Num: " << res.size();
	for(const auto& r : res)
	{
		std::cout << std::endl;
		PrintProgram(std::cout, r);
	}
	std::cout << std::endl << "Num: " << res.size();
#endif

	return 0;
}

