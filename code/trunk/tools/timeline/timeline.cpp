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

void writeFrame(std::ostream & out, size_t valid_from, size_t validity_range, 
				std::string frame_buffer1, std::string frame_buffer2, bool mark_newframe = true);




int main(int argc, char * argv[])
{
	using namespace std;
	if (argc < 2)
	{
		cout << "\nOne parameter is required: name of config file.\n";
		return (-1);
	}
	namespace bfs = boost::filesystem;
	using boost::format;
	using boost::io::group;

	bool verbose = false;
	int iverbose = 0;

	ifstream config_stream(argv[1]);
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

	config_stream >> master_path  
				  >> master_label
				  >> iverbose >> n_slaves;
	verbose = (iverbose != 0);


	ifstream master_line(master_path.external_file_string().c_str());

	masterTimeline  master;
	master.read(master_line);
	if (verbose)
		copy(master.m_timeline.begin(),master.m_timeline.end(),ostream_iterator<size_t>(cout,"\n"));
	master_line.close();

	typedef std::pair<slaveTimeline,std::string> namedSlaveLine;
	std::vector<namedSlaveLine>  v_slaveTimelines;

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
	string frame_buffer; 

	bool is_modified = true;
	size_t lastframe = 0;
	for (size_t k = 0 ; k < master.size(); k++)
	{
		size_t step = master.m_timeline[k];
		if (verbose)
			cout << "master frame " << k << " "  << step << endl;

		
		// loop over slave timelines
		BOOST_FOREACH(namedSlaveLine & i, v_slaveTimelines)
		{
			is_modified |= i.first.moveTo(step);
		}
		if (is_modified)
		{
			// close the previous frame
			if (k > 0 )
			{
				writeFrame(cout,lastframe, k - lastframe, frame_buffer, master_label);

			}
			lastframe = k;
			// new validity frame is requested
			// start new frame
			frame_buffer.clear();
			BOOST_FOREACH(namedSlaveLine i, v_slaveTimelines)
			{
				frame_buffer += (format(i.second) % i.first.m_current).str();
				frame_buffer += '\n';
			}
		}
		is_modified = false;
	}

	if (lastframe < master.size())
	{
		writeFrame(cout,lastframe, master.size() - lastframe, frame_buffer, master_label, false);
	}

	return 0;
}

void writeFrame( std::ostream & out, size_t valid_from, size_t validity_range, 
				std::string frame_buffer1, std::string frame_buffer2, bool mark_newframe )
{
	using namespace std;
	out << "% *** begin at frame " << valid_from << " ***" << endl;
	out << "% valid for " << validity_range << " frames (from " << valid_from 
		<< " to " << valid_from + validity_range - 1 << ")" << endl;
	out << "\\multiframe{" <<  validity_range << "}{i=" << valid_from << "+1}{%"<< endl;
	// minipage for slave
	out << "\\begin{minipage}{0.5\\textwidth}\n"
 		 << frame_buffer1 
		 << "\\end{minipage}" << endl;
	// minipage for master
	out << "\\begin{minipage}{0.5\\textwidth}\n"
		 << frame_buffer2 << '\n'
		 << "\\end{minipage}\n";
	out << "}\n"
		<< (mark_newframe ? "\\newframe\n" : "")
		<< "% *** end  of frame ***\n%%%%%"<<endl;

}
