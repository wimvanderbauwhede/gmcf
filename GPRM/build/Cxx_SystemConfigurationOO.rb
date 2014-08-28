# Cxx_SystemConfiguration.rb
#
# :title: Gannet Service-based SoC project - System Configuration module
#
# (c) 2004-2013 Wim Vanderbauwhede <@glasgow.ac.uk>
#
#==============================================================================
#
# Service-based SoC project - C++ System Configuration Generator
#
#==============================================================================
#

# FIXME: these constants must be identical to those in SBA/ServiceConfiguration.h

FS_SCLId = 16
FS_SCId = 8

require 'yaml'

if ENV.has_key?('GANNET_DIR')
    $LOAD_PATH.push("#{ENV['GANNET_DIR']}/Garnet/") # OBSOLETE!
else    
    raise "Please define GANNET_DIR"
end

help= <<EOH
    -Y, --yaml [YAML SystemConfiguration to be used as input]
    -D, --dir: relative path to directory for generated files
    -P, --procs: support processes+sockets
    -N, --new: for new features testing

EOH

require 'optparse'

opts=OptionParser.new()

$dirpath="./"
$sba_dir="./"

$sba_yml='SBA.yml'

WORDSZ=64
opts.on("-Y yml-file","--yml=yml-file",String) {|yml_file| $sba_yml=yml_file }
opts.on("-D dirpath","--dir=dirpath",String) {|dir_path| $dirpath=dir_path }
opts.on("-C cwd","--cwd=cwd",String) {|cwd| $sba_dir=cwd }
opts.on("-h","--help") {
puts help
exit
}
opts.parse(ARGV)

SBA_WD=$sba_dir

require 'yaml'
#require "SBA/ServiceConfiguration.rb"

def loadLibraryConfig(lib)
    puts "WD:#{SBA_WD}"
    if File.exists?("#{SBA_WD}/src/GPRM/Kernel/#{lib}.yml")
        libcfg=  YAML.load(File.open("#{SBA_WD}/src/GPRM/Kernel/#{lib}.yml"))
    elsif File.exists?("#{SBA_WD}/gensrc/#{lib}.yml")
        libcfg=  YAML.load(File.open("#{SBA_WD}/gensrc/#{lib}.yml"))
    elsif File.exists?("#{ENV['GANNET_DIR']}/GPRM/src/SBA/Base/#{lib}.yml")
        libcfg=  YAML.load(File.open("#{ENV['GANNET_DIR']}/GPRM/src/SBA/Base/#{lib}.yml"))
# FIXME: need to find a place for CoreServices.yml
    else
        raise "Can't find Library Config File #{lib}.yml"
    end
    return libcfg
end

=begin
  We need to read the Application config file from . (and only from .)
  Then we lookup up the Library config files in ./Gannet and $GANNET_DIR/SystemConfigurations
  Application config has ServiceNodes and Aliases
  Library config has Services and ServiceClasses
=end

    puts "GENERATING SystemConfiguration"
    appcfg =  YAML.load( File.open("#{$sba_yml}") )
    libs = appcfg['System']['Libraries']
    n_cfg=appcfg['System']['NServiceNodes'].to_i
    n_actual=appcfg['System']['ServiceNodes'].keys.length
    NServiceNodes= (n_actual < n_cfg)?n_cfg : n_actual
        
    i=0
    sclibs=[]
    libcfgs=[]
    for lib in libs
        libcfgs[i] =  loadLibraryConfig(lib)
                
        if File.exists?("#{SBA_WD}/src/GPRM/Kernel/#{lib}.h")
            sclibs[i]='#include "'+SBA_WD+'/src/GPRM/Kernel/'+lib+'.h"'
        elsif File.exists?("#{ENV['GANNET_DIR']}/GPRM/src/SBA/Base/#{lib}.h") 
#or 
#            File.exists?("#{ENV['GANNET_DIR']}/Garnet/SBA/ServiceCoreLibraries/#{lib}.h")
#            sclibs[i]='#include "SBA/ServiceCoreLibraries/'+lib+'.h"'
			sclibs[i]='#include "'+ENV['GANNET_DIR']+'/GanneVM/src/SBA/Base/'+lib+'.h' 
        else
            puts "WARNING: Can't find Library File #{lib}.h"
        end                            
        i+=1
    end
    
    def cxx_serviceclasses(appcfg,libcfgs) # OK
        cxx_service_tuples=[]
		prev_sclid_scid=0;
        for k in appcfg['System']['ServiceNodes'].keys
            entry=appcfg['System']['ServiceNodes'][k]
            node_id_str =entry[0]
            node_id = node_id_str.to_i            
            for scname in entry[1]                
                serviceclass=scname.split('.')
                sclid=1
                for libcfg in libcfgs
                    libn=libcfg['System']['Library']                        
                    if libn==serviceclass[0]
                        services= libcfg['System']['Services']
                        for sc_str in services.keys
                            if sc_str==serviceclass[1]
                                entry=services[sc_str]
                                service_id_str =entry[0]
                                scid = service_id_str.to_i            
                                core_method_name=entry[1]
                                sclid_scid=(sclid<<8)+scid
			        if sclid_scid!=prev_sclid_scid
	                            cxx_service_tuples.push("\t\tcase #{sclid_scid}:\n\t\t\t#{core_method_name}();\n\t\t\tbreak;" )     
				end	
				prev_sclid_scid=sclid_scid
                                break
                            end
                        end
                        break
                    end
                    sclid+=1
                end
            end
        end
        return cxx_service_tuples.join("\n")
    end  

            
    def cxx_serviceclass_constants(libcfgs) # OK
        serviceclass_constants=[]
        sclid=1
        for libcfg in libcfgs
            libn=libcfg['System']['Library']                        
#if libn==serviceclass[0]
                services=libcfg['System']['Services']
                for sc_str in services.keys
                    entry=services[sc_str]
# FIXME: rather ad-hoc, if it's a CTRL service there can be only one, if it's a COMP service the ServiceManager doesn't need to know
                    ctrl_or_comp=entry[2]
#if ctrl_or_comp.to_i==1
                    service_id_str =entry[0]
                    scid = service_id_str.to_i            
                    sclid_scid=(sclid<<8)+scid
                    sc_lib=libcfg['System']['Library']                
                    serviceclass_constants.push("const UINT SC_#{sc_lib}_#{sc_str} = #{sclid_scid};")                
#               end
#                end
            end
            sclid+=1
        end
        return serviceclass_constants.join("\n")
    end
    

    # FIXME: This is a hack for temporary compatibility. It assigns the node id to the first service class in the list
    # c3: [ 3, [LET] ]
    # This will break for multi-service nodes: c3: [3, Service1, LET, IF, ALU]
    def servicenodes(appcfg) # OK
        tServiceNodes={}
        for k in appcfg['System']['ServiceNodes'].keys
            entry=appcfg['System']['ServiceNodes'][k]
            node_id_str =entry[0]
            node_id = node_id_str.to_i
            if entry[1].length>1
                raise "Nodes with multiple service classes not yet supported"
            end
            service_class=entry[1][0]
            tServiceNodes[node_id]={}
            tServiceNodes[node_id]['Name']=service_class
            tServiceNodes[node_id]['Addr']=node_id
        end
#		puts tServiceNodes.inspect
        return tServiceNodes
    end        

    # FIXME: This is a hack for temporary compatibility. It assigns the node id to the first service class in the list
    # c3: [ 3, [LET] ]
    # This will break for multi-service nodes: c3: [3, Service1, LET, IF, ALU]
    def cxx_service_addresses(appcfg) # OK
        tServiceNodes=[]
        for k in appcfg['System']['ServiceNodes'].keys
            entry=appcfg['System']['ServiceNodes'][k]
            node_id_str =entry[0]
            tServiceNodes.push(node_id_str)
        end
        if tServiceNodes.length < NServiceNodes
        for k in tServiceNodes.length .. NServiceNodes-1
            tServiceNodes.push("1") # FIXME very ad-hoc
        end
        end
        return tServiceNodes.join(",")
    end        

    
    def cxx_method_constants(libcfgs) # OK
        method_constants=[]
        sclid=1
        for cfg in libcfgs    
            sc_lib=cfg['System']['Library']
            for serviceclass in cfg['System']['ServiceClasses'].keys
                scid=cfg['System']['Services'][serviceclass][0].to_i
                scid_field = scid  << FS_SCId
                sclid_field = sclid  << FS_SCLId
                opcode=1 # The compiler starts opcodes at 1, so 0 is an error code            
                for methname in  cfg['System']['ServiceClasses'][serviceclass]
                    method_constants.push("const UINT M_#{sc_lib}_#{serviceclass}_#{methname} = #{sclid_field+scid_field+opcode};")
                    opcode+=1
                end
            end
            sclid+=1
        end
        return method_constants.join("\n")
    end
    
    def cxx_servicenode_constants(appcfg,libcfgs) # OK
        
        servicenode_constants=[]
        for node_id in servicenodes(appcfg).keys
            servicenode_name_str = servicenodes(appcfg)[node_id]['Name']
            if servicenode_name_str=~/\.LET$/
                #eval "S_LET = #{node_id}"
                servicenode_constants.push("const UINT S_LET = #{node_id};")
            end
            if servicenode_name_str=~/\.IF$/            
                servicenode_constants.push("const UINT S_IF = #{node_id};")
            end            
            const_name_str=servicenode_name_str.sub('.','_') 
            serviceclass=servicenode_name_str.split('.')
            sc_lib=serviceclass[0]
            sc_str=serviceclass[1]
            for cfg in libcfgs
#			puts cfg.inspect
            if sc_lib==cfg['System']['Library']
                services=cfg['System']['Services']
                entry=services[sc_str]
#				puts entry.inspect
# FIXME: rather ad-hoc, if it's a CTRL service there can be only one, if it's a COMP service the ServiceManager doesn't need to know
                ctrl_or_comp=entry[2]
                if ctrl_or_comp.to_i==1
                    servicenode_constants.push("const UINT S_#{const_name_str} = #{node_id};")
                end
            end
            end
        end
        return servicenode_constants.join("\n")
    
    end # of cxx_servicenode_constants()

# ====================================================================  

cxxh_file="#{$dirpath}/SystemConfiguration.h"

cxxh=File.open(cxxh_file,"w")
    
cxxh.puts '
/** \file SystemConfiguration.h
   
 \brief Gannet Service-based SoC project - C++/SystemC System Configuration
        
        Generated from SBA.yml with create_Cxx_SystemConfiguration.rb

  (c) 2008-2012 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
    
*/

//==============================================================================
//
// System Configuration
//
// GENERATED from YAML configuration using create_Cxx_SystemConfiguration.rb
//
//==============================================================================

// 
'
cxxh.puts "
#ifndef _SBA_SYSTEM_CONFIGURATION_H_
#define _SBA_SYSTEM_CONFIGURATION_H_
"
cxxh.puts '
//#include <map>
'
#for libinclude in sclibs
#    cxxh.puts libinclude
#end

cxxh.puts "
using namespace std;

typedef unsigned int UINT;

namespace SBA {

"
cxxh.puts cxx_method_constants(libcfgs)
cxxh.puts cxx_servicenode_constants(appcfg,libcfgs)
cxxh.puts cxx_serviceclass_constants(libcfgs)    
cxxh.puts '
// Not elegant, but static arrays are a lot faster than linked lists!'
cxxh.puts "const UINT NSERVICES = #{NServiceNodes};"
cxxh.puts "const UINT SERVICE_ADDRESSES[#{NServiceNodes}]={#{cxx_service_addresses(appcfg)}};"
=begin
cxxh.puts '
/*
class Config {
    public: 
        ServiceMap services;
'
cxxh.puts '
    Config()
    {
'
cxxh.puts '#ifndef NO_SERVICES'
#cxxh.puts cxx_serviceclasses(appcfg,libcfgs)

cxxh.puts '#endif // NO_SERVICES'

cxxh.puts '
    };    
    
};
*/
'
=end
cxxh.puts "     
} // SBA
#endif /*_SBA_SYSTEM_CONFIGURATION_H_*/
"

cxxh.close           


cxx_file="#{$dirpath}/SelectWrapper.cc"

cxx=File.open(cxx_file,"w")
    
cxx.puts '
#include "Services.h"

void Services::select_wrapper(unsigned int code) {

	switch (code) {
'
cxx.puts cxx_serviceclasses(appcfg,libcfgs)	
cxx.puts '		default:
			none();
	};
}
'

cxx.close

