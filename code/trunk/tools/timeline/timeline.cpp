// timeline.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <boost/filesystem/path.hpp>
#include <boost/format.hpp>
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>

struct timePoint{
	timePoint() : m_step(0), m_variable(0.0,0.0) {}
	size_t	m_step;
	std::pair<double,double> m_variable;
};

std::istream & operator>>(std::istream & in, timePoint & x)
{
	in >> x.m_step >> x.m_variable.first >> x.m_variable.second;
	return in;
}

std::ostream & operator<<(std::ostream & out, const timePoint & x)
{
	out << x.m_step << ' ' << x.m_variable.first << ' ' << x.m_variable.second;
	return out;
}


template <typename value_type>
struct timeline
{
	timeline() : m_current(0) {}
	std::vector<value_type>		m_timeline;
	size_t						m_current;
	
	size_t						size() const {return m_timeline.size();}

	bool						moveTo(size_t step)
	{
		bool  is_moved = false;
		if (m_current == size() - 1)
			return is_moved;
		// shift
		while ((m_current < size() - 1) && (m_timeline[m_current+1].m_step < step))
		{
			m_current++;
			is_moved= true;
		}
		return is_moved;
	}

	int							read(std::istream & in)
	{
		size_t cntr;
		typename value_type step;
		while(in.good())
		{
			in >> cntr >> step;
			if (in.good())
				m_timeline.push_back(step);
		}
		return 0; 
	}
};


typedef	timeline<size_t>	masterTimeline;		
typedef timeline<timePoint>	slaveTimeline;

typedef std::pair<slaveTimeline,std::string> namedSlaveLine;
typedef std::vector<namedSlaveLine> slaveLineContainer;

enum OutputOrder
{
	masterFirst,
	slavesFirst
};


class OutputWriter{
public:

	OutputWriter(const std::string & master_label, const OutputOrder & order = masterFirst)
	:	m_master_label(master_label),
		m_lastframe(0),
		m_order(order)
	{

	}

	virtual void writeToCurrent(std::ostream & out, size_t master_frame_id, slaveLineContainer & v_slaveTimelines, bool is_modified)
	{
		// Start frame
		startFrame(out, master_frame_id, is_modified);
		writeFrame(out,master_frame_id,v_slaveTimelines,is_modified);
		endFrame(out, master_frame_id, is_modified);	
		// Update the state variables
		m_lastframe = master_frame_id;
	}

	virtual void initialize(std::ostream & out)
	{ /* Empty */ }

	virtual void finalize(std::ostream & out, size_t master_frame_id, slaveLineContainer & v_slaveTimelines, bool is_slave_updated)
	{ /* Empty */ }

protected:
	// Opens new frame, writes all necessary markups for new frame
	//!> It can mangle frame buffer
	virtual void startFrame(std::ostream & out, size_t master_frame_id, bool is_modified)
	{
		out << "<Frame range=[" << m_lastframe << ':' << master_frame_id << "]>\n";
		m_frame_buffer.clear();
	}

	//!> Closes the current frame, writes all necessary markups for closure of the frame
	//!> It can mangle frame buffer
	virtual void endFrame(std::ostream & out, size_t master_frame_id, bool is_modified)
	{
		out << m_frame_buffer;
		out << "</Frame>\n";
		m_frame_buffer.clear();
	}

	virtual void writeFrame(std::ostream & out, 
							size_t master_frame_id, 
							slaveLineContainer & v_slaveTimelines,
							bool is_slave_updated)
	{
	
		switch (m_order)
		{
		case masterFirst:
			writeMaster(out,master_frame_id);
			writeSlaves(out,v_slaveTimelines);
			break;
		case  slavesFirst:
			writeSlaves(out,v_slaveTimelines);
			writeMaster(out,master_frame_id);
			break;

		}
	}

	virtual void writeMaster(std::ostream &, size_t master_frame_id )
	{
		using boost::format;
		try
		{
			m_frame_buffer += (format(m_master_label) % master_frame_id).str();
		}
		catch(boost::io::format_error & )
		{
			// Quasi-normal error condition: master_label doesn't contain format string
			m_frame_buffer += m_master_label;
		}
		m_frame_buffer += "\n";
	}

	virtual void writeSlaves(std::ostream & out, slaveLineContainer & v_slaveTimelines )
	{
		BOOST_FOREACH(namedSlaveLine i, v_slaveTimelines)
		{
			writeSlave(out, i);
		}
	}


	virtual void writeSlave(std::ostream &, const namedSlaveLine &i )
	{
		using boost::format;
		try
		{
			m_frame_buffer += (format(i.second) % i.first.m_current).str();
		}
		catch(boost::io::format_error &)
		{
			// Quasi-normal error condition: slave label doesn't contain format string
			m_frame_buffer += i.second;
		}
		m_frame_buffer += '\n';
	}

	std::string m_frame_buffer; 
	std::string m_master_label;

	size_t		m_lastframe;	//!< Previous frame id

	OutputOrder m_order;
};



/*! Output format: compatible with LaTeX animate package, animateinline environment.
	Requirement: master frame label must contain %d format field.

	This is a special case of output format, it allows "external" iterations over 
	master states if a configuration of slave states is kept fixed for some time.
*/
class AnimateOutputWriter : public OutputWriter
{
public:
	AnimateOutputWriter(const std::string & master_label, const OutputOrder & order = masterFirst)
	:	OutputWriter(master_label,order)
		
	{

	}

	void writeToCurrent(std::ostream & out, size_t master_frame_id, slaveLineContainer & v_slaveTimelines, bool is_modified)
	{
		if (is_modified)
		{
			// flush pending frame
			if (master_frame_id > 0)
				OutputWriter::writeToCurrent(out,master_frame_id,v_slaveTimelines,is_modified);
			
			// start new pending frame (buffer only):
			//  Write slave state to the buffer
			OutputWriter::writeSlaves(out,v_slaveTimelines);
			m_slave_buffer = m_frame_buffer;
			m_frame_buffer.clear();
			m_lastframe  = master_frame_id;
		}
	}

	void finalize(std::ostream & out, size_t master_frame_id, slaveLineContainer & v_slaveTimelines, bool is_slave_updated)
	{ 
		if (m_lastframe < master_frame_id)
				OutputWriter::writeToCurrent(out,master_frame_id,v_slaveTimelines,true);
	}


protected:

	std::string		m_slave_buffer;

	void startFrame(std::ostream & out, size_t master_frame_id, bool is_modified)
	{
		
		size_t validity_range = master_frame_id - m_lastframe;

		out << "% *** begin at frame " << m_lastframe << " ***\n" ;
		out << "% valid for " << validity_range << " frames (from " << m_lastframe 
			<< " to " << m_lastframe + validity_range - 1 << ")\n";
		// for any frame except the first one, request explicitly new frame  
		if (m_lastframe != 0)
			out << "\\newframe\n";
		out << "\\multiframe{" <<  validity_range << "}{i=" << m_lastframe << "+1}{%"<< std::endl;
	}

	void writeMaster(std::ostream & out, size_t master_frame_id )
	{
		out << "\\begin{minipage}{0.5\\textwidth}\n";
		OutputWriter::writeMaster(out,master_frame_id);
		out << m_frame_buffer;
		m_frame_buffer.clear();
		out << "\\end{minipage}%\n";
		
	}

	 	
	void writeSlaves(std::ostream & out, slaveLineContainer & v_slaveTimelines )
	{
		out << "\\begin{minipage}{0.5\\textwidth}\n";
		// Just flush the buffer to the out
		out << m_slave_buffer;
		m_frame_buffer.clear();
		out << "\\end{minipage}%\n";
	}


	void endFrame(std::ostream & out, size_t master_frame_id, bool is_modified)
	{
		out << "}\n"  // end of multiframe
			<< "% *** end  of frame ***\n%%%%%"<<std::endl;
	}

};

/*! Output format: compatible with LaTeX beamer package.

*/
class FrameOutputWriter	:	public OutputWriter
{
public:
	FrameOutputWriter(const std::string & master_label, const OutputOrder & order = masterFirst)
	: OutputWriter(master_label,order)
	{

	}
protected:

	
	void startFrame(std::ostream & out, size_t master_frame_id, bool is_modified)
	{
		out << "% *** frame span:" << m_lastframe << " to " << master_frame_id  << " ***\n";
		out << "\\begin{frame}[fragile]\n";
		// minipage for slave/master
		if (m_order == slavesFirst)
		{
			// start minipage for slaves
			out << "\\begin{minipage}{0.5\\textwidth}\n";
		}
		// Clear buffer
		m_frame_buffer.clear();
	}

	void writeMaster(std::ostream & out, size_t master_frame_id )
	{
		if (m_order == slavesFirst)
		{
			out << m_frame_buffer;
			m_frame_buffer.clear();
			out << "\\end{minipage}%\n";
			// close slave minipage 
		}
		out << "\\begin{minipage}{0.5\\textwidth}\n";
		OutputWriter::writeMaster(out,master_frame_id);
		out << m_frame_buffer;
		m_frame_buffer.clear();
		out << "\\end{minipage}%\n";
		if (m_order == masterFirst)
		{
			// start minipage for slaves
			out << "\\begin{minipage}{0.5\\textwidth}\n";
		}
	}


	//!> Closes the current frame, writes all necessary markups for closure of the frame
	//!> It can mangle frame buffer
	virtual void endFrame(std::ostream & out, size_t master_frame_id, bool is_modified)
	{
		if (m_order == masterFirst)
		{
			out << m_frame_buffer;
			m_frame_buffer.clear();
			out << "\\end{minipage}\n";
			// close slave minipage 
		}
		out << "\\end{frame}\n";
		out << "% *** end  of frame ***\n%%%%%"<<std::endl;
		m_frame_buffer.clear();
	}

};


int main(int argc, char * argv[])
{
	using namespace std;
	
	namespace bfs = boost::filesystem;
	using boost::format;
	using boost::io::group;
	typedef int errcode;
	namespace po = boost::program_options;

	errcode info = 0;	
	po::options_description all_options("General options");

	all_options.add_options()
		("animate",		"Output for animate package")
		("frame",		"Output for regular beamer" )
		("plain",		"Plain output")
		;
	all_options.add_options()
		("help,h",		"Print help information")
		("input-file,f",po::value<std::string>(), "Input file")
		;

	po::variables_map vm;        

	try
	{	
		po::store(po::parse_command_line(argc, argv, all_options), vm);
		po::notify(vm);    
	}
	catch (boost::program_options::error e) 
	{
		cerr<<e.what()<<endl;
		return 1;
	}
	catch (std::exception e) 
	{
		cerr<<e.what()<<endl;
		return 1;
	}

	if ((argc < 3) || (vm.empty()) || (info = (errcode)vm.count("help")) ) 
	{
		all_options.print(cout);
		return (info == 0 ? 1 : 0);
	}

	if (vm.count("input-file") == 0)
	{
		cerr<<"No input file specified.\n";
		return 1;
	}
	string config_fname = vm["input-file"].as<std::string>();
	size_t mode_args = vm.count("plain") + vm.count("frame") + vm.count("animate");
	if (mode_args == 0)
	{
		cerr << "No run mode parameter.";
		return 1;
	}
	if (mode_args > 1)
	{
		cerr << "Too many mode parameters, one is expected, " <<mode_args << " are given" << endl;;
		return 1;
	}


	//////////////////////////////////////////////////////////////////////////
	bool verbose = false;
	int itmp = 0;

	ifstream config_stream(config_fname.c_str());
	if (config_stream.fail())
	{
		cerr << "Cannot open config file " << argv[1] << endl;
		return (1);
	}

	//////////////////////////////////////////////////////////////////////////
	// configuration
	bfs::path master_path;
	size_t n_slaves;
	bfs::path slave_path;
	string master_label, slave_label;
	string master_pos;
	OutputOrder order = masterFirst;

	config_stream >> master_path;
	getline(config_stream,master_label);
	config_stream >> master_pos;
	if (master_pos == "master")
		order = masterFirst;
	else 
	if (master_pos == "slaves")
		order = slavesFirst;
	else
	{
		std::cerr << "Bad line in config file:" << master_pos << std::endl;
		return 1;
	}
	config_stream >> itmp >> n_slaves;
	verbose = (itmp != 0);

	ifstream master_line(master_path.external_file_string().c_str());

	masterTimeline  master;
	master.read(master_line);
	if (verbose)
		copy(master.m_timeline.begin(),master.m_timeline.end(),ostream_iterator<size_t>(cout,"\n"));
	master_line.close();

	slaveLineContainer  v_slaveTimelines;

	

	ifstream slave_line;
	timePoint zero_time;
	for(size_t i = 0; i < n_slaves; i++)
	{
		
		config_stream >> slave_path;
		getline(config_stream,slave_label);
		slave_line.open(slave_path.canonize().external_file_string().c_str());
		if (verbose)
			cout << "slave " << i << " : " << slave_path << " "<< slave_label <<endl;
		if (slave_line.fail())
		{
			cerr<<"cannot read slave line."<<endl;
			continue;
		}
		slaveTimeline slave;
		slave.m_timeline.push_back(zero_time);
		//
		slave.read(slave_line);
		//slave.m_current = 1;
		if (verbose)
			copy(slave.m_timeline.begin(),slave.m_timeline.end(),ostream_iterator<timePoint>(cout,"\n"));
		v_slaveTimelines.push_back(std::pair<slaveTimeline,std::string>(slave,slave_label));
		slave_line.close();
		slave_line.clear();
	}

	// loop over master timeline
	OutputWriter * ptr_writer = NULL;
	// create output writer object
	if (vm.count("plain"))
		ptr_writer =  new OutputWriter(master_label,order);
	else
	if (vm.count("frame"))
		ptr_writer =  new FrameOutputWriter(master_label,order);	
	else
	if (vm.count("animate"))
		ptr_writer =  new FrameOutputWriter(master_label,order);	
	
	if (!ptr_writer)
	{
		cerr << "Cannot determine output type" << endl;
		return 1;
	}

	//OutputWriter fw(master_label,slavesFirst);
	//FrameOutputWriter fw(master_label,slavesFirst);
	//AnimateOutputWriter fw(master_label,slavesFirst);
	//AnimateOutputWriter fw(master_label,masterFirst);
	ptr_writer->initialize(cout);
	//AnimateOutputWriter fw(master_label);

	bool any_slave_modified = true;
	size_t lastframe = 0;
	for (size_t master_frame = 0 ; master_frame < master.size(); master_frame++)
	{
		size_t step = master.m_timeline[master_frame];
		if (verbose)
			cout << "%%% master frame " << master_frame << " "  << step << endl;
		// Advance slave lines up to the current step
		// Test if any slave state was actually advanced.	
		BOOST_FOREACH(namedSlaveLine & i, v_slaveTimelines)
		{
			any_slave_modified |= i.first.moveTo(step);
		}
		ptr_writer->writeToCurrent(cout, master_frame, v_slaveTimelines,any_slave_modified);
		any_slave_modified = false;
	}

	ptr_writer->finalize(cout,master.size(),v_slaveTimelines,any_slave_modified);

	return 0;
}



