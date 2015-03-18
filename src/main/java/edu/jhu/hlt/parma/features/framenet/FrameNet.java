// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.framenet;

import edu.jhu.hlt.parma.util.ParmaConfig;

import java.io.*;
import java.util.*;

public class FrameNet implements Serializable {

	private static FrameNet singleton;

	public static final String FEATURES_FRAMENET_PREDS_TO_FRAMES_DATAPATH = "features.framenet.predtoframes_datapath";
	public static final String FEATURES_FRAMENET_FRAMES_TO_PREDS_DATAPATH = "features.framenet.frametopreds_datapath";
	/*public static final String FEATURES_FRAMENET_FRAMES_TO_ROLES_DATAPATH = "features.framenet.frametoroles_datapath";
    public static final String FEATURES_FRAMENET_PREDS_TO_VALENCES_DATAPATH = "features.framenet.predtovalences_datapath";
	 */public static final String FEATURES_FRAMENET_FRAMES_TO_PARENTS_DATAPATH ="features.framenet.frametoparents_datapath";
	 public static final String FEATURES_FRAMENET_FRAMES_TO_CHILDREN_DATAPATH = "features.framenet.frametochildren_datapath";
	 public static final String FEATURES_FRAMENET_FRAMES_TO_PERSPECTIVE_PARENTS_DATAPATH = "features.framenet.frametoperspectiveparents_datapath";
	 public static final String FEATURES_FRAMENET_FRAMES_TO_PERSPECTIVE_CHILDREN_DATAPATH = "features.framenet.frametoperspectivechildren_datapath";



	 private HashMap<String,Set<String>> predicatesToFrames = new HashMap<String,Set<String>>();
	 private HashMap<String,Set<String>> framesToPredicates = new HashMap<String,Set<String>>();
	 /*private HashMap<String,Set<List<String>>> framesToRoles = new HashMap<String,Set<List<String>>>();
    private HashMap<String,Set<List<String>>> predicatesToValences = new HashMap<String,Set<List<String>>>();
	  */private HashMap<String,Set<String>> framesToParents = new HashMap<String,Set<String>>();
	  private HashMap<String,Set<String>> framesToChildren = new HashMap<String,Set<String>>();
	  private HashMap<String,Set<String>> framesToPerspectiveParents = new HashMap<String,Set<String>>();
	  private HashMap<String,Set<String>> framesToPerspectiveChildren = new HashMap<String,Set<String>>();


	  public static FrameNet getInstance() {
		  if(singleton == null) {
			  try { singleton = new FrameNet(); }
			  catch (IOException e){
				  throw new RuntimeException(e);
			  }
		  }
		  return singleton;
	  }


	  private FrameNet() throws IOException { 
		  String predToFramesPath = ParmaConfig.getFile(FEATURES_FRAMENET_PREDS_TO_FRAMES_DATAPATH).getPath();
		  String frameToPredsPath = ParmaConfig.getFile(FEATURES_FRAMENET_FRAMES_TO_PREDS_DATAPATH).getPath();
		  String frameToParentsPath = ParmaConfig.getFile(FEATURES_FRAMENET_FRAMES_TO_PARENTS_DATAPATH).getPath();
		  String frameToChildrenPath = ParmaConfig.getFile(FEATURES_FRAMENET_FRAMES_TO_CHILDREN_DATAPATH).getPath();
		  String frameToPerspectiveParentsPath = ParmaConfig.getFile(FEATURES_FRAMENET_FRAMES_TO_PERSPECTIVE_PARENTS_DATAPATH).getPath();
		  String frameToPerspectiveChildrenPath = ParmaConfig.getFile(FEATURES_FRAMENET_FRAMES_TO_PERSPECTIVE_CHILDREN_DATAPATH).getPath();

		  System.out.println("loading wordnet data from " + predToFramesPath + " " + frameToPredsPath + " " + 
				  /*frameToRolesPath + " " + predToValencesPath*/
				  frameToParentsPath + " " + frameToChildrenPath + " " +
				  frameToPerspectiveParentsPath + " " +
				  frameToPerspectiveChildrenPath);

		  loadData(predToFramesPath, frameToPredsPath, /*frameToRolesPath, predToValencesPath*/ frameToParentsPath, frameToChildrenPath, frameToPerspectiveParentsPath, frameToPerspectiveChildrenPath);
	  }

	  private void loadData(String predToFramePath, String frameToPredsPath,
			  /*String frameToRolePath, String predToValencesPath*/
			  String frameToParentsPath, String frameToChildrenPath,
			  String frameToPerspectiveParentsPath, String frameToPerspectiveChildrenPath) throws IOException {
		  BufferedReader buf;
		  buf = new BufferedReader(new FileReader(predToFramePath));
		  while (buf.ready()){
			  String line = buf.readLine();
			  String[] parts = line.split("\\t");
			  if (predicatesToFrames.get(parts[0]) == null)
				  predicatesToFrames.put(parts[0],new HashSet<String>());
			  predicatesToFrames.get(parts[0]).add(parts[1]);
		  }

		  buf = new BufferedReader(new FileReader(frameToPredsPath));
		  while (buf.ready()){
			  String line = buf.readLine();
			  String[] parts = line.split("\\t");
			  if (framesToPredicates.get(parts[0]) == null)
				  framesToPredicates.put(parts[0],new HashSet<String>());
			  framesToPredicates.get(parts[0]).add(parts[1]);
		  }

		  buf = new BufferedReader(new FileReader(frameToParentsPath));
		  while (buf.ready()){
			  String line = buf.readLine();
			  String[] parts = line.split("\\t");
			  if (framesToParents.get(parts[0]) == null)
				  framesToParents.put(parts[0],new HashSet<String>());
			  framesToParents.get(parts[0]).add(parts[1]);
		  }

		  buf = new BufferedReader(new FileReader(frameToChildrenPath));
		  while (buf.ready()){
			  String line = buf.readLine();
			  String[] parts = line.split("\\t");
			  if (framesToChildren.get(parts[0]) == null)
				  framesToChildren.put(parts[0],new HashSet<String>());
			  framesToChildren.get(parts[0]).add(parts[1]);
		  }

		  buf = new BufferedReader(new FileReader(frameToPerspectiveParentsPath));
		  while (buf.ready()){
			  String line = buf.readLine();
			  String[] parts = line.split("\\t");
			  if (framesToPerspectiveParents.get(parts[0]) == null)
				  framesToPerspectiveParents.put(parts[0],new HashSet<String>());
			  framesToPerspectiveParents.get(parts[0]).add(parts[1]);
		  }

		  buf = new BufferedReader(new FileReader(frameToPerspectiveChildrenPath));
		  while (buf.ready()){
			  String line = buf.readLine();
			  String[] parts = line.split("\\t");
			  if (framesToPerspectiveChildren.get(parts[0]) == null)
				  framesToPerspectiveChildren.put(parts[0],new HashSet<String>());
			  framesToPerspectiveChildren.get(parts[0]).add(parts[1]);
		  }

	  }

	  public boolean sharedFrame(String first, String second){
		  Set<String> frames = predicatesToFrames.get(first);
		  if (frames != null) {
			  for (String frame : frames){
				  if ((framesToPredicates.get(frame) != null) && framesToPredicates.get(frame).contains(second))
					  return true;
			  }
		  }
		  return false;
	  }

	  /*    public boolean parentFrame(String first, String second){
        Set<String> frames = predicatesToFrames.get(first);
        if (frames != null) {
            for (String frame : frames){
                if (framesToParents.get(frame) != null) {
                    Set<String> parents = framesToParents.get(frame);
                    for (String parent : parents) {
                        if ((framesToPredicates.get(parent) != null) && framesToPredicates.get(parent).contains(second))
                            return true;
                    }
		}
            }
        }
        return false;
    }

    public boolean childFrame(String first, String second){
        Set<String> frames = predicatesToFrames.get(first);
        if (frames != null) {
            for (String frame : frames){
                if (framesToChildren.get(frame) != null) {
                    Set<String> children = framesToChildren.get(frame);
                    for (String child : children) {
                        if ((framesToPredicates.get(child) != null) && framesToPredicates.get(child).contains(second))
                            return true;
                    }
		}
            }
        }
        return false;
    }

    public boolean perspectiveParentFrame(String first, String second){
        Set<String> frames = predicatesToFrames.get(first);
        if (frames != null) {
            for (String frame : frames){
                if (framesToPerspectiveParents.get(frame) != null) {
                    Set<String> perspectiveParents = framesToPerspectiveParents.get(frame);
                    for (String parent : perspectiveParents) {
                        if ((framesToPredicates.get(parent) != null) && framesToPredicates.get(parent).contains(second))
                            return true;
                    }
		}
            }
        }
        return false;
    }

    public boolean perspectiveChildFrame(String first, String second){
        Set<String> frames = predicatesToFrames.get(first);
        if (frames != null) {
            for (String frame : frames){
                if (framesToPerspectiveChildren.get(frame) != null) {
                    Set<String> perspectiveChildren = framesToChildren.get(frame);
                    for (String child : perspectiveChildren) {
                        if ((framesToPredicates.get(child) != null) && framesToPredicates.get(child).contains(second))
                            return true;
                    }
		}
            }
        }
        return false;
    }
	   */
	  private static final String TYPE_PARENT = "TYPE_PARENT";
	  private static final String TYPE_CHILD = "TYPE_CHILD";
	  private static final String TYPE_PERSPECTIVE_PARENT = "TYPE_PERSPECTIVE_PARENT";
	  private static final String TYPE_PERSPECTIVE_CHILD = "TYPE_PERSPECTIVE_CHILD";
	  private static final String TYPE_MIXED = "TYPE_MIXED";

	  //container calls for distance features: read e.g. "the second string is a parent/child/etc. of the first by n steps"
	  public int getParentDistance(String first, String second, int maxDistance){
		  return getDistance(first, second, maxDistance, TYPE_PARENT);
	  }

	  public int getChildDistance(String first, String second, int maxDistance){
		  return getDistance(first, second, maxDistance, TYPE_CHILD);
	  }

	  public int getPerspectiveParentDistance(String first, String second, int maxDistance){
		  return getDistance(first, second, maxDistance, TYPE_PERSPECTIVE_PARENT);
	  }

	  public int getPerspectiveChildDistance(String first, String second, int maxDistance){
		  return getDistance(first, second, maxDistance, TYPE_PERSPECTIVE_CHILD);
	  }

	  public int getMixedDistance(String first, String second, int maxDistance){
		  return getDistance(first, second, maxDistance, TYPE_MIXED);
	  }

	  public int getDistance(String first, String second, int maxDistance, String type){
		  assert(first != null);
		  assert(second != null);
		  Set<String> visitedFrameSet = new HashSet<String>();
		  Queue<StringIntPair> queue = new LinkedList<StringIntPair>();
		  Set<String> potentialFrames = predicatesToFrames.get(first);
		  if(potentialFrames == null) return maxDistance + 1;
		  for (String frame : potentialFrames) {
			  queue.add(new StringIntPair(frame, 1));
		  }
		  while(!queue.isEmpty() && queue.peek().i < maxDistance){
			  StringIntPair sip = queue.remove();
			  if (!visitedFrameSet.contains(sip.s)){
				  Set<String> neighbors = framesToPredicates.get(sip.s);
				  if (neighbors != null) {
					  for (String neighbor : neighbors) {
						  if (neighbor.equals(second)){
							  return sip.i;
						  }
					  }
				  }
				  visitedFrameSet.add(sip.s);	
				  if (sip.i < maxDistance) {
					  for (String frame : nextFrame(sip.s, type)) {
						  queue.add(new StringIntPair(frame, sip.i+1));
					  }
				  }
			  }
		  }
		  return maxDistance + 1;
	  }

	  private Set<String> nextFrame(String frame, String type) {
		  assert(frame != null);
		  assert(type != null);
		  Set<String> connected = new HashSet<String>();
		  if ("TYPE_PARENT".equals(type) || "TYPE_MIXED".equals(type) ) {
			  Set<String> frames = framesToParents.get(frame);
			  if (frames != null)
				  connected.addAll(frames);
		  }
		  if ("TYPE_CHILD".equals(type) || "TYPE_MIXED".equals(type) ) {
			  Set<String> frames = framesToChildren.get(frame);
			  if (frames != null)
				  connected.addAll(frames);
		  }
		  if ("TYPE_PERSPECTIVE_PARENT".equals(type) || "TYPE_MIXED".equals(type) ) {
			  Set<String> frames = framesToPerspectiveParents.get(frame);
			  if (frames != null)
				  connected.addAll(frames);
		  }
		  if ("TYPE_PERSPECTIVE_CHILD".equals(type) || "TYPE_MIXED".equals(type) ) {
			  Set<String> frames = framesToPerspectiveChildren.get(frame);
			  if (frames != null)
				  connected.addAll(frames);
		  }
		  return connected;
	  }

	  private class StringIntPair{
		  public String s;
		  public int i;
		  public StringIntPair(String s, int i) {
			  this.s = s;
			  this.i = i;
		  }
	  }
}
